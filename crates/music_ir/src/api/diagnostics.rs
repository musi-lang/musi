use music_base::diag::Diag;

use crate::IrDiagKind;

pub type IrDiagList = Vec<Diag>;

#[must_use]
pub fn ir_diag_kind(diag: &Diag) -> Option<IrDiagKind> {
    IrDiagKind::from_diag(diag)
}
