mod effects;
mod expr;
mod ffi;
mod imports;
mod locals;
mod patterns;
mod top_level;
mod types;

use std::collections::{HashMap, HashSet};

use music_ast::common::{MemberDecl, MemberName, Param};
use music_ast::expr::{
    BinOp, CaseArm, CompClause, ExprKind, FStrPart, FieldTarget, HandlerClause, ImportKind,
    InstanceBody, InstanceDef, LetBinding, PostfixOp, QuoteKind, RecordField, SpliceKind,
    TypeOpKind, UnaryOp,
};
use music_ast::pat::{PatKind, RecordPatField};
use music_ast::ty::TyKind;
use music_ast::{AttrId, ExprId, ExprList, PatId, TyId};
use music_hir::{AttrSpec, TypedModule, attr_expr_string, attr_path_matches, bind_attr};
use music_il::format::{
    self, ClassDescriptor, ClassInstance, ClassMethod, EffectDescriptor, EffectOpDescriptor,
    FfiType, ForeignAbi, ForeignDescriptor, TypeDescriptor, TypeKind,
};
use music_il::instruction::{Instruction, Operand};
use music_il::opcode::Opcode;
use music_owned::types::BuiltinType;
use music_resolve::def::{DefKind, Visibility};
use music_sema::{DispatchInfo, NominalKey, SemaTypeId, Ty, TypeKey};
use music_shared::{Ident, Literal, Symbol, SymbolList};

use crate::error::EmitError;
use crate::pool::{ConstantEntry, ConstantPool};

type EmitResult<T = ()> = Result<T, EmitError>;

const LINK_ATTR: AttrSpec = AttrSpec {
    path: &["link"],
    params: &["name", "symbol"],
    required: &[],
};

pub struct MethodEntry {
    pub name: Option<Symbol>,
    pub source_name: Option<String>,
    pub instructions: Vec<Instruction>,
    pub locals_count: u16,
    pub absolute_global_loads: Vec<usize>,
}

pub struct GlobalEntry {
    pub name: Symbol,
    pub source_name: String,
    pub exported: bool,
    pub opaque: bool,
}

pub struct SeamModule {
    pub constants: ConstantPool,
    pub methods: Vec<MethodEntry>,
    pub globals: Vec<GlobalEntry>,
    pub types: Vec<TypeDescriptor>,
    pub effects: Vec<EffectDescriptor>,
    pub classes: Vec<ClassDescriptor>,
    pub foreigns: Vec<ForeignDescriptor>,
}

struct Emitter<'module> {
    typed_module: &'module TypedModule,
    pool: ConstantPool,
    methods: Vec<MethodEntry>,
    globals: Vec<GlobalEntry>,
    types: Vec<TypeDescriptor>,
    type_ids: HashMap<String, u16>,
    foreigns: Vec<ForeignDescriptor>,
    foreign_globals: HashMap<Symbol, u16>,
    imported_globals: HashMap<String, u16>,
    module_exports: HashMap<Symbol, Vec<ImportedGlobal>>,
    current_instructions: Vec<Instruction>,
    current_absolute_global_loads: Vec<usize>,
    current_locals: SymbolList,
    current_upvalues: SymbolList,
    cell_locals: HashSet<Symbol>,
    next_anon: u32,
    next_type_id: u16,
}

pub fn emit(typed_module: &TypedModule) -> EmitResult<SeamModule> {
    emit_with_context(typed_module, HashMap::new())
}

#[expect(clippy::implicit_hasher, reason = "always called with default HashMap")]
pub fn emit_with_context(
    typed_module: &TypedModule,
    module_exports: HashMap<Symbol, Vec<ImportedGlobal>>,
) -> EmitResult<SeamModule> {
    let mut emitter = Emitter {
        typed_module,
        pool: ConstantPool::new(),
        methods: Vec::new(),
        globals: Vec::new(),
        types: builtin_type_descriptors(),
        type_ids: builtin_type_ids(),
        foreigns: Vec::new(),
        foreign_globals: HashMap::new(),
        imported_globals: HashMap::new(),
        module_exports,
        current_instructions: Vec::new(),
        current_absolute_global_loads: Vec::new(),
        current_locals: Vec::new(),
        current_upvalues: Vec::new(),
        cell_locals: HashSet::new(),
        next_anon: u32::MAX - 1,
        next_type_id: format::FIRST_EMITTED_TYPE_ID,
    };
    emitter.emit_module()?;
    let effects = emitter.collect_effects();
    let classes = emitter.collect_classes();
    Ok(SeamModule {
        constants: emitter.pool,
        methods: emitter.methods,
        globals: emitter.globals,
        types: emitter.types,
        effects,
        classes,
        foreigns: emitter.foreigns,
    })
}

fn builtin_type_descriptors() -> Vec<TypeDescriptor> {
    BuiltinType::ALL
        .iter()
        .map(|bt| TypeDescriptor {
            id: bt.type_id(),
            key: bt.name().to_owned(),
            kind: TypeKind::Builtin,
            member_count: 0,
        })
        .collect()
}

fn builtin_type_ids() -> HashMap<String, u16> {
    BuiltinType::ALL
        .iter()
        .map(|bt| (bt.name().to_owned(), bt.type_id()))
        .collect()
}

#[derive(Debug, Clone)]
pub struct ImportedGlobal {
    pub name: String,
    pub index: u16,
}

const fn instruction_byte_size(instr: &Instruction) -> usize {
    1 + match &instr.operand {
        Operand::None => 0,
        Operand::U8(_) => 1,
        Operand::I16(_) | Operand::U16(_) => 2,
        Operand::Wide(_, _) => 3,
        Operand::TypeLen(_, _) => 4,
        Operand::TypeTagged(_, _, _) => 5,
        Operand::Effect(_, _) => 4,
        Operand::IndexedJump(_, _) => 4,
        Operand::EffectJump(_, _, _) => 6,
        Operand::Table(offsets) => 2 + offsets.len() * 2,
    }
}

const fn member_name_symbol(name: &MemberName) -> Symbol {
    match name {
        MemberName::Ident(ident) | MemberName::Op(ident, _) => ident.name,
    }
}

const fn resolve_visibility(binding: &LetBinding) -> Visibility {
    if binding.modifiers.opaque {
        Visibility::Opaque
    } else if binding.modifiers.exported {
        Visibility::Exported
    } else {
        Visibility::Private
    }
}

const fn dynamic_binop_opcodes(op: BinOp) -> Option<(Opcode, Opcode)> {
    match op {
        BinOp::Add => Some((Opcode::IAdd, Opcode::FAdd)),
        BinOp::Sub => Some((Opcode::ISub, Opcode::FSub)),
        BinOp::Shl => Some((Opcode::Shl, Opcode::Shl)),
        BinOp::Shr => Some((Opcode::Shr, Opcode::Shr)),
        BinOp::Mul => Some((Opcode::IMul, Opcode::FMul)),
        BinOp::Div => Some((Opcode::IDiv, Opcode::FDiv)),
        BinOp::Rem => Some((Opcode::IRem, Opcode::IRem)),
        BinOp::Eq => Some((Opcode::CmpEq, Opcode::CmpEq)),
        BinOp::NotEq => Some((Opcode::CmpNeq, Opcode::CmpNeq)),
        BinOp::Lt => Some((Opcode::CmpLt, Opcode::CmpLt)),
        BinOp::Gt => Some((Opcode::CmpGt, Opcode::CmpGt)),
        BinOp::LtEq => Some((Opcode::CmpLeq, Opcode::CmpLeq)),
        BinOp::GtEq => Some((Opcode::CmpGeq, Opcode::CmpGeq)),
        BinOp::And
        | BinOp::Or
        | BinOp::Xor
        | BinOp::Cons
        | BinOp::NilCoalesce
        | BinOp::PipeRight
        | BinOp::Range
        | BinOp::RangeExcl => None,
    }
}

#[expect(
    clippy::panic,
    reason = "NilCoalesce/PipeRight/Range/RangeExcl are intercepted by callers — reaching this arm is a compiler ICE"
)]
fn binop_to_opcode(op: BinOp) -> Opcode {
    match op {
        BinOp::Add => Opcode::IAdd,
        BinOp::Sub => Opcode::ISub,
        BinOp::Shl => Opcode::Shl,
        BinOp::Shr => Opcode::Shr,
        BinOp::Mul => Opcode::IMul,
        BinOp::Div => Opcode::IDiv,
        BinOp::Rem => Opcode::IRem,
        BinOp::Eq => Opcode::CmpEq,
        BinOp::NotEq => Opcode::CmpNeq,
        BinOp::Lt => Opcode::CmpLt,
        BinOp::Gt => Opcode::CmpGt,
        BinOp::LtEq => Opcode::CmpLeq,
        BinOp::GtEq => Opcode::CmpGeq,
        BinOp::And => Opcode::And,
        BinOp::Or => Opcode::Or,
        BinOp::Xor => Opcode::Xor,
        BinOp::Cons => Opcode::ArrCaten,
        BinOp::NilCoalesce | BinOp::PipeRight | BinOp::Range | BinOp::RangeExcl => {
            panic!("intercepted before reaching binop_to_opcode")
        }
    }
}
