use super::*;

type RecordPatFieldRange = SliceRange<HirRecordPatField>;

#[derive(Clone, Copy)]
struct IrrefutablePatInput<'a> {
    origin: IrOrigin,
    import_record_target: Option<&'a ModuleKey>,
}

impl IrrefutablePatInput<'_> {
    fn lower_bindings(
        self,
        ctx: &mut LowerCtx<'_>,
        pat: HirPatId,
        base: IrExpr,
        out: &mut Vec<IrExpr>,
    ) -> Result<(), Box<str>> {
        let sema = ctx.sema;
        match &sema.module().store.pats.get(pat).kind {
            HirPatKind::Error | HirPatKind::Wildcard => {}
            HirPatKind::Bind { name } => self.lower_bind(ctx, *name, base, out),
            HirPatKind::As { pat: inner, name } => {
                self.lower_as(ctx, *inner, *name, base, out)?;
            }
            HirPatKind::Tuple { items } | HirPatKind::Array { items } => {
                self.lower_sequence(ctx, *items, base, out)?;
            }
            HirPatKind::Record { fields } => {
                self.lower_record(ctx, fields.clone(), pat, base, out)?;
            }
            other => return Err(format!("invalid local let pattern in lowering: {other:?}").into()),
        }
        Ok(())
    }

    fn lower_bind(self, ctx: &LowerCtx<'_>, name: Ident, base: IrExpr, out: &mut Vec<IrExpr>) {
        let sema = ctx.sema;
        let interner = ctx.interner;
        out.push(IrExpr::new(
            self.origin,
            IrExprKind::Let {
                binding: decl_binding_id(sema, name),
                name: interner.resolve(name.name).into(),
                value: Box::new(base),
            },
        ));
    }

    fn lower_as(
        self,
        ctx: &mut LowerCtx<'_>,
        inner: HirPatId,
        name: Ident,
        base: IrExpr,
        out: &mut Vec<IrExpr>,
    ) -> Result<(), Box<str>> {
        let sema = ctx.sema;
        let interner = ctx.interner;
        let stored = store_in_temp(ctx, self.origin, base, out);
        out.push(IrExpr::new(
            self.origin,
            IrExprKind::Let {
                binding: decl_binding_id(sema, name),
                name: interner.resolve(name.name).into(),
                value: Box::new(stored.clone()),
            },
        ));
        self.lower_bindings(ctx, inner, stored, out)
    }

    fn lower_sequence(
        self,
        ctx: &mut LowerCtx<'_>,
        items: SliceRange<HirPatId>,
        base: IrExpr,
        out: &mut Vec<IrExpr>,
    ) -> Result<(), Box<str>> {
        let sema = ctx.sema;
        let stored = store_in_temp(ctx, self.origin, base, out);
        for (index, item) in sema.module().store.pat_ids.get(items).iter().enumerate() {
            let Some(proj) = project_index_expr(self.origin, stored.clone(), index) else {
                continue;
            };
            self.lower_bindings(ctx, *item, proj, out)?;
        }
        Ok(())
    }

    fn lower_record(
        self,
        ctx: &mut LowerCtx<'_>,
        fields: RecordPatFieldRange,
        pat: HirPatId,
        base: IrExpr,
        out: &mut Vec<IrExpr>,
    ) -> Result<(), Box<str>> {
        let sema = ctx.sema;
        let pat_ty = sema.try_pat_ty(pat).unwrap_or_else(|| {
            lowering_invariant_violation("pattern type missing for destructuring")
        });
        if self.import_record_target.is_some() {
            return self.lower_module_record(ctx, fields, out);
        }
        match &sema.ty(pat_ty).kind {
            HirTyKind::Record { .. } => self.lower_value_record(ctx, fields, pat_ty, base, out),
            _ => Err(super::lower_errors::lowering_error(
                "record destructuring without record base",
            )),
        }
    }

    fn lower_module_record(
        self,
        ctx: &mut LowerCtx<'_>,
        fields: RecordPatFieldRange,
        out: &mut Vec<IrExpr>,
    ) -> Result<(), Box<str>> {
        let sema = ctx.sema;
        let interner = ctx.interner;
        let Some(import_record_target) = self.import_record_target else {
            return Err(super::lower_errors::lowering_error(
                "import record destructuring without import record target",
            ));
        };
        for field in sema.module().store.record_pat_fields.get(fields) {
            let name_text: Box<str> = interner.resolve(field.name.name).into();
            let proj = IrExpr::new(
                self.origin,
                IrExprKind::Name {
                    binding: None,
                    name: name_text.clone(),
                    import_record_target: Some(import_record_target.clone()),
                },
            );
            self.lower_record_field(ctx, field, proj, name_text, out)?;
        }
        Ok(())
    }

    fn lower_value_record(
        self,
        ctx: &mut LowerCtx<'_>,
        fields: RecordPatFieldRange,
        pat_ty: HirTyId,
        base: IrExpr,
        out: &mut Vec<IrExpr>,
    ) -> Result<(), Box<str>> {
        let sema = ctx.sema;
        let interner = ctx.interner;
        let stored = store_in_temp(ctx, self.origin, base, out);
        let Some((indices, _layout, _field_count)) = record_layout_for_ty(sema, pat_ty, interner)
        else {
            return Err(super::lower_errors::lowering_error(
                "record destructuring without record layout",
            ));
        };
        for field in sema.module().store.record_pat_fields.get(fields) {
            let Some(index) = indices.get(interner.resolve(field.name.name)).copied() else {
                return Err(super::lower_errors::lowering_error(
                    "record destructuring missing field",
                ));
            };
            let proj = IrExpr::new(
                self.origin,
                IrExprKind::RecordGet {
                    base: Box::new(stored.clone()),
                    index,
                },
            );
            let name_text: Box<str> = interner.resolve(field.name.name).into();
            self.lower_record_field(ctx, field, proj, name_text, out)?;
        }
        Ok(())
    }

    fn lower_record_field(
        self,
        ctx: &mut LowerCtx<'_>,
        field: &HirRecordPatField,
        proj: IrExpr,
        name: Box<str>,
        out: &mut Vec<IrExpr>,
    ) -> Result<(), Box<str>> {
        if let Some(value_pat) = field.value {
            return self.lower_bindings(ctx, value_pat, proj, out);
        }
        out.push(IrExpr::new(
            self.origin,
            IrExprKind::Let {
                binding: decl_binding_id(ctx.sema, field.name),
                name,
                value: Box::new(proj),
            },
        ));
        Ok(())
    }
}

pub(super) fn lower_destructure_let(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    pat: HirPatId,
    value: HirExprId,
) -> Result<IrExprKind, Box<str>> {
    let import_record_target = ctx.sema.expr_import_record_target(value);

    let value_expr = lower_expr(ctx, value);
    let temp = fresh_temp(ctx);
    let mut exprs = Vec::<IrExpr>::new();
    exprs.push(IrExpr::new(
        origin,
        IrExprKind::TempLet {
            temp,
            value: Box::new(value_expr),
        },
    ));

    let base = IrExpr::new(origin, IrExprKind::Temp { temp });
    IrrefutablePatInput {
        origin,
        import_record_target,
    }
    .lower_bindings(ctx, pat, base, &mut exprs)?;
    exprs.push(IrExpr::new(origin, IrExprKind::Unit));
    Ok(IrExprKind::Sequence {
        exprs: exprs.into_boxed_slice(),
    })
}

fn project_index_expr(origin: IrOrigin, base: IrExpr, index: usize) -> Option<IrExpr> {
    let Ok(index_u32) = u32::try_from(index) else {
        return None;
    };
    Some(IrExpr::new(
        origin,
        IrExprKind::Index {
            base: Box::new(base),
            indices: vec![IrExpr::new(
                origin,
                IrExprKind::Lit(IrLit::Int {
                    raw: index_u32.to_string().into(),
                }),
            )]
            .into_boxed_slice(),
        },
    ))
}

fn store_in_temp(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    base: IrExpr,
    out: &mut Vec<IrExpr>,
) -> IrExpr {
    let temp = fresh_temp(ctx);
    out.push(IrExpr::new(
        origin,
        IrExprKind::TempLet {
            temp,
            value: Box::new(base),
        },
    ));
    IrExpr::new(origin, IrExprKind::Temp { temp })
}
