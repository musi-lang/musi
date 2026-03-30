use super::*;
use crate::checker::Checker;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UnifyMismatch {
    pub left: SemTyId,
    pub right: SemTyId,
}

#[must_use]
pub fn resolve(tys: &SemTys, ty: SemTyId) -> SemTyId {
    match tys.get(ty) {
        SemTy::InferVar(var) => tys
            .infer_binding(*var)
            .map(|b| resolve(tys, b))
            .unwrap_or(ty),
        _ => ty,
    }
}

pub fn unify(tys: &mut SemTys, left: SemTyId, right: SemTyId) -> Result<SemTyId, UnifyMismatch> {
    let left = resolve(tys, left);
    let right = resolve(tys, right);
    if left == right {
        return Ok(left);
    }

    match (tys.get(left).clone(), tys.get(right).clone()) {
        (SemTy::Error, _) | (_, SemTy::Error) => Ok(tys.alloc(SemTy::Error)),
        (SemTy::Unknown, _) | (_, SemTy::Unknown) => Ok(tys.alloc(SemTy::Unknown)),
        (SemTy::Any, _) | (_, SemTy::Any) => Ok(tys.alloc(SemTy::Any)),

        (SemTy::InferVar(var), _) => {
            tys.bind_infer(var, right);
            Ok(right)
        }
        (_, SemTy::InferVar(var)) => {
            tys.bind_infer(var, left);
            Ok(left)
        }

        (SemTy::Generic(a), SemTy::Generic(b)) if a == b => Ok(left),

        (
            SemTy::Named {
                name: a,
                args: a_args,
            },
            SemTy::Named {
                name: b,
                args: b_args,
            },
        ) if a == b && a_args.len() == b_args.len() => {
            for (a, b) in a_args.iter().copied().zip(b_args.iter().copied()) {
                let _ = unify(tys, a, b)?;
            }
            Ok(left)
        }
        (SemTy::Tuple { items: a }, SemTy::Tuple { items: b }) if a.len() == b.len() => {
            for (a, b) in a.iter().copied().zip(b.iter().copied()) {
                let _ = unify(tys, a, b)?;
            }
            Ok(left)
        }
        (
            SemTy::Array {
                dims: a_dims,
                elem: a_elem,
            },
            SemTy::Array {
                dims: b_dims,
                elem: b_elem,
            },
        ) => {
            let _ = unify(tys, a_elem, b_elem)?;
            if !array_dims_compatible(&a_dims, &b_dims) {
                return Err(UnifyMismatch { left, right });
            }
            Ok(select_array_type(left, right, &a_dims, &b_dims))
        }
        (
            SemTy::Arrow {
                flavor: a_flavor,
                input: a_in,
                output: a_out,
            },
            SemTy::Arrow {
                flavor: b_flavor,
                input: b_in,
                output: b_out,
            },
        ) => {
            let _ = unify(tys, a_in, b_in)?;
            let _ = unify(tys, a_out, b_out)?;
            if a_flavor == b_flavor {
                return Ok(left);
            }
            // Allow `T -> U` to unify with `T ~> U` by keeping the effectful flavor.
            match (a_flavor, b_flavor) {
                (music_hir::HirArrowFlavor::Effectful, music_hir::HirArrowFlavor::Pure) => Ok(left),
                (music_hir::HirArrowFlavor::Pure, music_hir::HirArrowFlavor::Effectful) => {
                    Ok(right)
                }
                _ => Ok(left),
            }
        }
        (
            SemTy::Binary {
                op: a_op,
                left: a_l,
                right: a_r,
            },
            SemTy::Binary {
                op: b_op,
                left: b_l,
                right: b_r,
            },
        ) if a_op == b_op => {
            let _ = unify(tys, a_l, b_l)?;
            let _ = unify(tys, a_r, b_r)?;
            Ok(left)
        }
        (SemTy::Mut { base: a }, SemTy::Mut { base: b }) => {
            let _ = unify(tys, a, b)?;
            Ok(left)
        }
        (SemTy::Record { fields: a }, SemTy::Record { fields: b }) if a.keys().eq(b.keys()) => {
            for (k, a_ty) in a {
                let b_ty = b.get(&k).copied().expect("record keys already matched");
                let _ = unify(tys, a_ty, b_ty)?;
            }
            Ok(left)
        }

        _ => Err(UnifyMismatch { left, right }),
    }
}

fn array_dims_compatible(left: &[music_hir::HirDim], right: &[music_hir::HirDim]) -> bool {
    if left.len() != right.len() {
        return false;
    }

    for (a, b) in left.iter().zip(right.iter()) {
        match (a, b) {
            (
                music_hir::HirDim::IntLit { value: Some(a), .. },
                music_hir::HirDim::IntLit { value: Some(b), .. },
            ) if a != b => return false,
            _ => {}
        }
    }

    true
}

fn array_specificity(dims: &[music_hir::HirDim]) -> u32 {
    let mut score = 0u32;
    for d in dims {
        match d {
            music_hir::HirDim::IntLit { value: Some(_), .. } => score += 2,
            music_hir::HirDim::Name { .. } => score += 1,
            music_hir::HirDim::Inferred { .. } | music_hir::HirDim::IntLit { value: None, .. } => {}
        }
    }
    score
}

fn select_array_type(
    left_id: SemTyId,
    right_id: SemTyId,
    left_dims: &[music_hir::HirDim],
    right_dims: &[music_hir::HirDim],
) -> SemTyId {
    if array_specificity(right_dims) > array_specificity(left_dims) {
        right_id
    } else {
        left_id
    }
}

impl<'a> Checker<'a> {
    fn read_ty(&self, ty: SemTyId) -> SemTyId {
        let mut ty = resolve(&self.state.semtys, ty);
        loop {
            let SemTy::Mut { base } = self.state.semtys.get(ty).clone() else {
                return ty;
            };
            ty = resolve(&self.state.semtys, base);
        }
    }

    pub(crate) fn unify_or_report(
        &mut self,
        span: music_basic::Span,
        expected: SemTyId,
        found: SemTyId,
    ) -> SemTyId {
        let expected = resolve(&self.state.semtys, expected);
        let mut found = resolve(&self.state.semtys, found);
        if !matches!(self.state.semtys.get(expected), SemTy::Mut { .. }) {
            found = self.read_ty(found);
        }

        match unify(&mut self.state.semtys, expected, found) {
            Ok(ok) => ok,
            Err(m) => {
                let expected = SemTyDisplay {
                    tys: &self.state.semtys,
                    interner: self.ctx.interner,
                    ty: m.left,
                }
                .to_string();
                let found = SemTyDisplay {
                    tys: &self.state.semtys,
                    interner: self.ctx.interner,
                    ty: m.right,
                }
                .to_string();
                self.error(span, SemaErrorKind::TypeMismatch { expected, found });
                self.state.builtins.error
            }
        }
    }

    pub(crate) fn unify_read_or_report(
        &mut self,
        span: music_basic::Span,
        left: SemTyId,
        right: SemTyId,
    ) -> SemTyId {
        let left = self.read_ty(left);
        let right = self.read_ty(right);
        self.unify_or_report(span, left, right)
    }
}
