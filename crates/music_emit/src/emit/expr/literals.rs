use super::super::*;
use crate::EmitDiagKind;
use music_term::SyntaxTerm;

impl MethodEmitter<'_, '_> {
    pub(super) fn compile_lit(&mut self, lit: &IrLit, origin: &IrOrigin, diags: &mut EmitDiagList) {
        match lit {
            IrLit::Int { raw } => self.compile_int_literal(raw, origin, diags),
            IrLit::Float { raw } => self.compile_float_literal(raw, origin, diags),
            IrLit::String { value } => self.compile_string_literal(value),
            IrLit::Rune { value } => self.compile_i64(i64::from(*value)),
        }
    }

    pub(super) fn compile_string_constant(&mut self, value: &str) {
        let string_id = self.artifact.intern_string(value);
        let const_name = format!("const:string:{}", self.artifact.constants.len());
        let name_id = self.artifact.intern_string(&const_name);
        let constant_id = self.artifact.constants.alloc(ConstantDescriptor::new(
            name_id,
            ConstantValue::String(string_id),
        ));
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdConst,
            Operand::Constant(constant_id),
        )));
    }

    pub(super) fn compile_syntax_constant(
        &mut self,
        raw: &str,
        origin: &IrOrigin,
        diags: &mut EmitDiagList,
    ) {
        let Ok(term) = SyntaxTerm::from_quote_source(raw) else {
            super::support::push_expr_diag(
                diags,
                self.module_key,
                origin,
                EmitDiagKind::InvalidSyntaxLiteral,
                format!("invalid syntax literal `{raw}`"),
            );
            emit_zero(self);
            return;
        };
        let text_id = self.artifact.intern_string(term.text());
        let const_name = format!("const:syntax:{}", self.artifact.constants.len());
        let name_id = self.artifact.intern_string(&const_name);
        let constant_id = self.artifact.constants.alloc(ConstantDescriptor::new(
            name_id,
            ConstantValue::Syntax {
                shape: term.shape(),
                text: text_id,
            },
        ));
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdConst,
            Operand::Constant(constant_id),
        )));
    }

    fn compile_int_literal(&mut self, raw: &str, origin: &IrOrigin, diags: &mut EmitDiagList) {
        if let Some(value) = parse_int_literal(raw) {
            self.compile_i64(value);
        } else {
            super::support::push_expr_diag(
                diags,
                self.module_key,
                origin,
                EmitDiagKind::InvalidIntegerLiteral,
                format!("invalid integer literal `{raw}`"),
            );
            emit_zero(self);
        }
    }

    fn compile_string_literal(&mut self, value: &str) {
        self.compile_string_constant(value);
    }

    fn compile_float_literal(&mut self, raw: &str, origin: &IrOrigin, diags: &mut EmitDiagList) {
        let compact = raw.replace('_', "");
        let Ok(value) = compact.parse::<f64>() else {
            super::support::push_expr_diag(
                diags,
                self.module_key,
                origin,
                EmitDiagKind::InvalidFloatLiteral,
                format!("invalid float literal `{raw}`"),
            );
            emit_zero(self);
            return;
        };
        let const_name = format!("const:float:{}", self.artifact.constants.len());
        let name_id = self.artifact.intern_string(&const_name);
        let constant_id = self.artifact.constants.alloc(ConstantDescriptor::new(
            name_id,
            ConstantValue::Float(value),
        ));
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdConst,
            Operand::Constant(constant_id),
        )));
    }

    pub(super) fn compile_i64(&mut self, value: i64) {
        if let Ok(short) = i16::try_from(value) {
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::LdSmi,
                Operand::I16(short),
            )));
            return;
        }
        let const_name = format!("const:int:{}", self.artifact.constants.len());
        let name_id = self.artifact.intern_string(&const_name);
        let constant_id = self
            .artifact
            .constants
            .alloc(ConstantDescriptor::new(name_id, ConstantValue::Int(value)));
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdConst,
            Operand::Constant(constant_id),
        )));
    }
}

fn parse_int_literal(raw: &str) -> Option<i64> {
    let compact = raw.replace('_', "");
    let (sign, digits) = compact
        .strip_prefix('-')
        .map_or((1_i64, compact.as_str()), |rest| (-1_i64, rest));
    let (radix, digits) = digits
        .strip_prefix("0x")
        .or_else(|| digits.strip_prefix("0X"))
        .map_or_else(
            || {
                digits
                    .strip_prefix("0o")
                    .or_else(|| digits.strip_prefix("0O"))
                    .map_or_else(
                        || {
                            digits
                                .strip_prefix("0b")
                                .or_else(|| digits.strip_prefix("0B"))
                                .map_or((10, digits), |rest| (2, rest))
                        },
                        |rest| (8, rest),
                    )
            },
            |rest| (16, rest),
        );
    i64::from_str_radix(digits, radix)
        .ok()
        .map(|value| value * sign)
}
