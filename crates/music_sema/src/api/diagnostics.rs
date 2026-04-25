use music_base::diag::Diag;

use crate::diag::SemaDiagKind;

pub type SemaDiagList = Vec<Diag>;

#[must_use]
pub fn sema_diag_kind(diag: &Diag) -> Option<SemaDiagKind> {
    SemaDiagKind::from_diag(diag)
}
