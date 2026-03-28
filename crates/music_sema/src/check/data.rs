use music_ast::ExprId;
use music_ast::common::MemberDecl;
use music_ast::expr::{DataBody, ExprKind};
use music_ast::pat::PatKind;
use music_owned::prelude::{BUILTIN_VARIANTS, PRELUDE_CLASSES};
use music_owned::types::BuiltinType;
use music_shared::Ident;

use super::{SemaDb, VariantDefInfo};
use crate::types::{SemaTypeId, Ty};

impl SemaDb {
    pub(super) fn register_data_variants(&mut self, body: &DataBody) {
        if let DataBody::Sum(variants) = body {
            for (i, v) in variants.iter().enumerate() {
                let tag_index = u16::try_from(i).expect("too many variants (>65535)");
                let arity = u8::from(v.payload.is_some());
                let _ = self
                    .variant_registry
                    .insert(v.name.name, VariantDefInfo { tag_index, arity });
                let _ = self.env.variant_tags.insert(v.name.name, tag_index);
            }
        }
    }

    pub(super) fn seed_prelude_intrinsics(&mut self) {
        for class in PRELUDE_CLASSES {
            for method in class.methods {
                let Some(opcode) = method.opcode else {
                    continue;
                };
                let symbol = self.db.interner.intern(method.op_name);
                let _ = self.intrinsic_methods.insert(symbol, opcode);
            }
        }

        for variant in BUILTIN_VARIANTS {
            let symbol = self.db.interner.intern(variant.name);
            let _ = self.intrinsic_variants.insert(symbol, variant.opcode);
            let _ = self.env.variant_tags.insert(symbol, variant.tag_index);
            let _ = self.variant_registry.insert(
                symbol,
                VariantDefInfo {
                    tag_index: variant.tag_index,
                    arity: variant.arity,
                },
            );
        }
    }

    pub(super) fn synth_variant_lit(
        &mut self,
        tag: &Ident,
        args: &[ExprId],
        expr_id: ExprId,
    ) -> SemaTypeId {
        for &arg in args {
            let _ = self.synth(arg);
        }
        let is_builtin_bool = matches!(self.db.interner.resolve(tag.name), "True" | "False");
        if let Some(&opcode) = self.intrinsic_variants.get(&tag.name) {
            let _ = self
                .env
                .dispatch
                .insert(expr_id, crate::env::DispatchInfo::Static { opcode });
        }
        if let Some(&vdef) = self.variant_registry.get(&tag.name) {
            let parent_type = if is_builtin_bool {
                self.env.builtin(BuiltinType::Bool)
            } else {
                self.env.intern(Ty::Any)
            };
            let _ = self.env.variant_info.insert(
                expr_id,
                crate::env::VariantInfo {
                    parent_type,
                    tag_index: vdef.tag_index,
                    arity: vdef.arity,
                },
            );
        }
        if is_builtin_bool {
            self.env.builtin(BuiltinType::Bool)
        } else {
            self.env.intern(Ty::Any)
        }
    }

    /// Registers effect operation metadata in the type environment.
    pub(super) fn synth_effect_def(&mut self, members: &[MemberDecl]) -> SemaTypeId {
        let root = self.db.ast.root.clone();
        for &expr_id in &root {
            let spanned = self.db.ast.exprs.get(expr_id);
            if let ExprKind::Let(ref binding) = spanned.kind {
                if let Some(value_id) = binding.value {
                    let val = self.db.ast.exprs.get(value_id);
                    if matches!(val.kind, ExprKind::EffectDef(_)) {
                        let pat = self.db.ast.pats.get(binding.pat);
                        if let PatKind::Bind(bind_ident) = &pat.kind {
                            let effect_name = bind_ident.name;
                            let operations = members
                                .iter()
                                .filter_map(|m| match m {
                                    MemberDecl::Fn(fn_decl) => {
                                        let param_ty = fn_decl.params.as_ref().map(|params| {
                                            if params.len() == 1 {
                                                params[0]
                                                    .ty
                                                    .map(|ty_id| self.lower_ty(ty_id))
                                                    .unwrap_or_else(|| self.env.intern(Ty::Any))
                                            } else {
                                                let elems = params
                                                    .iter()
                                                    .map(|param| {
                                                        param
                                                            .ty
                                                            .map(|ty_id| self.lower_ty(ty_id))
                                                            .unwrap_or_else(|| {
                                                                self.env.intern(Ty::Any)
                                                            })
                                                    })
                                                    .collect();
                                                self.env.intern(Ty::Tuple(elems))
                                            }
                                        });
                                        let ret_ty = fn_decl
                                            .ret_ty
                                            .map(|ty_id| self.lower_ty(ty_id))
                                            .unwrap_or_else(|| self.env.intern(Ty::Unit));
                                        Some((
                                            super::expr::member_name_symbol(&fn_decl.name),
                                            param_ty,
                                            ret_ty,
                                        ))
                                    }
                                    MemberDecl::Law(_) => None,
                                })
                                .collect();
                            let _ = self.env.register_effect_ops(effect_name, operations);
                        }
                    }
                }
            }
        }

        self.env.intern(Ty::Builtin(BuiltinType::Type))
    }
}
