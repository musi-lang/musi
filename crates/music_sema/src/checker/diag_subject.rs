use music_hir::{HirBinaryOp, HirExprId, HirExprKind, HirLitId, HirLitKind, HirPrefixOp};

use super::PassBase;

impl PassBase<'_, '_, '_> {
    pub fn expr_subject(&self, id: HirExprId) -> String {
        let expr = self.expr(id);
        match &expr.kind {
            HirExprKind::Error => "error".to_owned(),
            HirExprKind::Name { name } | HirExprKind::Field { name, .. } => {
                self.resolve_symbol(name.name).to_owned()
            }
            HirExprKind::Lit { lit } => self.lit_subject(*lit),
            HirExprKind::Variant { tag, .. } => self.resolve_symbol(tag.name).to_owned(),
            HirExprKind::Prefix { op, .. } => prefix_op_subject(op).to_owned(),
            HirExprKind::Binary { op, .. } => self.binary_op_subject(op),
            _ => expr_kind_subject(&expr.kind).to_owned(),
        }
    }

    fn lit_subject(&self, id: HirLitId) -> String {
        match self.lit(id).kind {
            HirLitKind::Int { raw } | HirLitKind::Float { raw } => raw.into(),
            HirLitKind::String { value } => format!("\"{value}\""),
            HirLitKind::Rune { value } => format!("U+{value:04X}"),
        }
    }

    pub fn binary_op_subject(&self, op: &HirBinaryOp) -> String {
        match op {
            HirBinaryOp::Range {
                include_lower,
                include_upper,
            } => range_op_subject(*include_lower, *include_upper).to_owned(),
            HirBinaryOp::In => "in".to_owned(),
            HirBinaryOp::UserOp(ident) => self.resolve_symbol(ident.name).to_owned(),
            _ => fixed_binary_op_subject(op).to_owned(),
        }
    }
}

const fn fixed_binary_op_subject(op: &HirBinaryOp) -> &'static str {
    match op {
        HirBinaryOp::Assign => "=",
        HirBinaryOp::Arrow => "->",
        HirBinaryOp::TypeEq | HirBinaryOp::Eq => "==",
        HirBinaryOp::Or => "or",
        HirBinaryOp::Xor => "xor",
        HirBinaryOp::And => "and",
        HirBinaryOp::Ne => "!=",
        HirBinaryOp::Lt => "<",
        HirBinaryOp::Gt => ">",
        HirBinaryOp::Le => "<=",
        HirBinaryOp::Ge => ">=",
        HirBinaryOp::Shl => "<<",
        HirBinaryOp::Shr => ">>",
        HirBinaryOp::Add => "+",
        HirBinaryOp::Sub => "-",
        HirBinaryOp::Mul => "*",
        HirBinaryOp::Div => "/",
        HirBinaryOp::Rem => "%",
        HirBinaryOp::In | HirBinaryOp::Range { .. } | HirBinaryOp::UserOp(_) => "operator",
    }
}

const fn range_op_subject(include_lower: bool, include_upper: bool) -> &'static str {
    match (include_lower, include_upper) {
        (true, true) => "..",
        (true, false) => "..<",
        (false, true) => "<..",
        (false, false) => "<..<",
    }
}

fn expr_kind_subject(kind: &HirExprKind) -> &'static str {
    composite_expr_subject(kind).unwrap_or_else(|| decl_expr_subject(kind))
}

const fn composite_expr_subject(kind: &HirExprKind) -> Option<&'static str> {
    match kind {
        HirExprKind::Template { .. } => Some("template literal"),
        HirExprKind::Sequence { .. } => Some("sequence expression"),
        HirExprKind::Tuple { .. } => Some("tuple expression"),
        HirExprKind::Array { .. } => Some("array expression"),
        HirExprKind::ArrayTy { .. } => Some("array type expression"),
        HirExprKind::Record { .. } => Some("record expression"),
        HirExprKind::Pi { .. } => Some("callable type expression"),
        HirExprKind::Lambda { .. } => Some("lambda expression"),
        HirExprKind::Call { .. } => Some("call expression"),
        HirExprKind::Apply { .. } => Some("type application"),
        HirExprKind::Index { .. } => Some("index expression"),
        HirExprKind::RecordUpdate { .. } => Some("record update"),
        HirExprKind::TypeTest { .. } => Some("type test"),
        HirExprKind::TypeCast { .. } => Some("type cast"),
        HirExprKind::PartialRange { .. } => Some("partial range"),
        _ => None,
    }
}

const fn decl_expr_subject(kind: &HirExprKind) -> &'static str {
    match kind {
        HirExprKind::Let { .. } => "let expression",
        HirExprKind::Import { .. } => "import expression",
        HirExprKind::Yield { .. } => "yield expression",
        HirExprKind::Defer { .. } => "defer expression",
        HirExprKind::Match { .. } => "match expression",
        HirExprKind::If { .. } => "if expression",
        HirExprKind::Data { .. } => "data declaration",
        HirExprKind::Shape { .. } => "shape declaration",
        HirExprKind::Unsafe { .. } => "unsafe expression",
        HirExprKind::Pin { .. } => "pin expression",
        _ => "expression",
    }
}

const fn prefix_op_subject(op: &HirPrefixOp) -> &'static str {
    match op {
        HirPrefixOp::Neg => "-",
        HirPrefixOp::Not => "not",
        HirPrefixOp::Mut => "mut",
        HirPrefixOp::Known => "known",
    }
}
