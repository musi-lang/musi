use std::collections::{HashMap, HashSet};

use musi_ast::{ChoiceVariant, Expr, Ty, VariantPayload};
use musi_shared::{Arena, Idx, Interner};

use crate::error::CodegenError;
use crate::intrinsics;
use crate::{FunctionEntry, Module, Opcode, SymbolEntry, SymbolFlags};

// -- TypeTag ------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub(super) enum TypeTag {
    Int = 0,
    Float = 1,
    Str = 2,
    Unit = 3,
    Array = 7,
}

impl TypeTag {
    pub(super) fn from_type_name(name: &str) -> Option<Self> {
        match name {
            "Int" | "Int8" | "Int16" | "Int32" | "Int64"
            | "Nat" | "Nat8" | "Nat16" | "Nat32" | "Nat64" => Some(Self::Int),
            "Float" | "Float32" | "Float64" => Some(Self::Float),
            "String" => Some(Self::Str),
            "Unit" => Some(Self::Unit),
            _ => None,
        }
    }
}

// -- LoopCtx + FnEmitter ------------------------------------------------------

pub(super) struct LoopCtx {
    pub(super) start_pos: usize,
    pub(super) break_fixups: Vec<usize>,
}

pub(super) struct FnEmitter {
    pub(super) code: Vec<u8>,
    pub(super) scopes: Vec<HashMap<String, u16>>,
    pub(super) next_slot: u16,
    pub(super) loop_stack: Vec<LoopCtx>,
    pub(super) local_types: HashMap<u16, String>,
    /// Maps local slot → field names in declaration order, for anonymous records.
    pub(super) anon_layouts: HashMap<u16, Vec<String>>,
}

impl FnEmitter {
    pub(super) const BR: u8 = 0x60;
    pub(super) const BR_TRUE: u8 = 0x61;
    pub(super) const BR_FALSE: u8 = 0x62;

    pub(super) fn new() -> Self {
        Self {
            code: Vec::new(),
            scopes: Vec::new(),
            next_slot: 0,
            loop_stack: Vec::new(),
            local_types: HashMap::new(),
            anon_layouts: HashMap::new(),
        }
    }

    pub(super) fn push_scope(&mut self) { self.scopes.push(HashMap::new()); }
    pub(super) fn pop_scope(&mut self) { let _d = self.scopes.pop(); }

    pub(super) fn define_local(&mut self, name: &str) -> Result<u16, CodegenError> {
        let slot = self.next_slot;
        self.next_slot = self.next_slot.checked_add(1).ok_or(CodegenError::TooManyLocals)?;
        let scope = self.scopes.last_mut().expect("define_local called with no active scope");
        let _prev = scope.insert(name.to_owned(), slot);
        Ok(slot)
    }

    pub(super) fn lookup_local(&self, name: &str) -> Option<u16> {
        for scope in self.scopes.iter().rev() {
            if let Some(&slot) = scope.get(name) { return Some(slot); }
        }
        None
    }

    pub(super) fn push(&mut self, op: &Opcode) { op.encode_into(&mut self.code); }
    pub(super) const fn len(&self) -> usize { self.code.len() }

    pub(super) fn emit_jump_placeholder(&mut self, opcode_tag: u8) -> usize {
        let pos = self.code.len();
        self.code.push(opcode_tag);
        self.code.extend_from_slice(&i32::MAX.to_le_bytes());
        pos
    }

    pub(super) fn patch_jump_to_here(&mut self, fixup_pos: usize) -> Result<(), CodegenError> {
        let after_instr = fixup_pos.checked_add(5).expect("fixup_pos + 5 fits usize");
        let target = self.code.len();
        let offset = i32::try_from(
            isize::try_from(target).map_err(|_| CodegenError::JumpOffsetOverflow)?
                .wrapping_sub(isize::try_from(after_instr).map_err(|_| CodegenError::JumpOffsetOverflow)?),
        ).map_err(|_| CodegenError::JumpOffsetOverflow)?;
        self.code[fixup_pos + 1..fixup_pos + 5].copy_from_slice(&offset.to_le_bytes());
        Ok(())
    }

    pub(super) fn emit_br_back(&mut self, target_pos: usize) -> Result<(), CodegenError> {
        let after_instr = self.code.len().checked_add(5).expect("code len + 5 fits usize");
        let offset = i32::try_from(
            isize::try_from(target_pos).map_err(|_| CodegenError::JumpOffsetOverflow)?
                .wrapping_sub(isize::try_from(after_instr).map_err(|_| CodegenError::JumpOffsetOverflow)?),
        ).map_err(|_| CodegenError::JumpOffsetOverflow)?;
        self.push(&Opcode::Br(offset));
        Ok(())
    }

    pub(super) fn start_loop(&mut self) -> usize {
        let start_pos = self.len();
        self.loop_stack.push(LoopCtx { start_pos, break_fixups: Vec::new() });
        start_pos
    }

    pub(super) fn pop_loop(&mut self) -> LoopCtx {
        self.loop_stack.pop().expect("pop_loop called with no active loop")
    }

    pub(super) fn current_loop_start(&self) -> Option<usize> {
        self.loop_stack.last().map(|ctx| ctx.start_pos)
    }

    pub(super) fn push_break_fixup(&mut self, fixup_pos: usize) {
        self.loop_stack.last_mut().expect("push_break_fixup called outside loop")
            .break_fixups.push(fixup_pos);
    }

    pub(super) fn close_loop(&mut self) -> Result<(), CodegenError> {
        let ctx = self.pop_loop();
        for pos in ctx.break_fixups { self.patch_jump_to_here(pos)?; }
        self.push(&Opcode::LdImmUnit);
        Ok(())
    }

    pub(super) const fn local_count(&self) -> u16 { self.next_slot }
    pub(super) fn flush_into(self, buf: &mut Vec<u8>) { buf.extend_from_slice(&self.code); }
    pub(super) const fn byte_len(&self) -> usize { self.code.len() }
}

// -- EmitArenas ---------------------------------------------------------------

pub(super) struct EmitArenas<'a> {
    pub(super) exprs: &'a Arena<Expr>,
    pub(super) interner: &'a Interner,
}

// -- Type info ----------------------------------------------------------------

pub(super) struct TypeInfo {
    pub(super) field_names: Vec<String>,
}

#[derive(Clone)]
pub(super) struct VariantInfo {
    pub(super) type_name: String,
    pub(super) discriminant: i64,
    pub(super) payload_count: u16,
    pub(super) total_field_count: u16,
}

pub(super) struct PendingLambda {
    pub(super) fn_idx: u16,
    pub(super) params: Vec<(String, Option<String>)>,
    pub(super) body: Idx<Expr>,
}

pub(super) struct EmitState {
    pub(super) fn_map: HashMap<String, u16>,
    pub(super) fn_return_types: HashMap<String, String>,
    pub(super) type_map: HashMap<String, TypeInfo>,
    pub(super) variant_map: HashMap<String, VariantInfo>,
    pub(super) lambda_counter: u16,
    pub(super) pending_lambdas: Vec<PendingLambda>,
    pub(super) class_method_names: HashSet<String>,
    pub(super) type_tag_map: HashMap<String, u16>,
    pub(super) next_type_tag: u16,
}

// -- Free helpers -------------------------------------------------------------

pub(super) fn ty_name_str(ty: &Ty, interner: &Interner) -> Option<String> {
    if let Ty::Named { name, .. } = ty {
        return Some(interner.resolve(*name).to_owned());
    }
    None
}

pub(super) fn param_list_with_types(
    params: &[musi_ast::Param],
    interner: &Interner,
) -> Vec<(String, Option<String>)> {
    params.iter().map(|p| {
        let name = interner.resolve(p.name).to_owned();
        let type_name = p.ty.as_ref().and_then(|t| ty_name_str(t, interner));
        (name, type_name)
    }).collect()
}

pub(super) fn payload_count(v: &ChoiceVariant) -> u16 {
    match &v.payload {
        None | Some(VariantPayload::Discriminant(_)) => 0,
        Some(VariantPayload::Positional(tys)) => u16::try_from(tys.len()).unwrap_or(u16::MAX),
        Some(VariantPayload::Named(fields)) => u16::try_from(fields.len()).unwrap_or(u16::MAX),
    }
}

// -- Shared emitter helpers ---------------------------------------------------

/// Creates a plain (non-native, non-export) symbol + function entry and returns the function index.
pub(super) fn push_plain_fn(
    name: &str,
    param_count: u8,
    module: &mut Module,
) -> Result<u16, CodegenError> {
    let sym_idx = module.push_symbol(SymbolEntry {
        name: name.into(),
        flags: SymbolFlags::new(0),
        intrinsic_id: intrinsics::NONE_ID,
        abi: Box::from(""),
        link_lib: None,
        link_name: None,
    })?;
    module.push_function(FunctionEntry {
        symbol_idx: sym_idx,
        param_count,
        local_count: 0,
        code_offset: 0,
        code_length: 0,
    })
}

/// Registers a closure/lambda/inner-fn: creates the fn entry, inserts into `fn_map`,
/// and schedules the body for emission. Returns the function index.
pub(super) fn register_fn_closure(
    name: &str,
    params_with_types: Vec<(String, Option<String>)>,
    body: Idx<Expr>,
    state: &mut EmitState,
    module: &mut Module,
) -> Result<u16, CodegenError> {
    let param_count = u8::try_from(params_with_types.len())
        .map_err(|_| CodegenError::ParameterCountOverflow)?;
    let fn_idx = push_plain_fn(name, param_count, module)?;
    let _prev = state.fn_map.insert(name.to_owned(), fn_idx);
    state.pending_lambdas.push(PendingLambda { fn_idx, params: params_with_types, body });
    Ok(fn_idx)
}
