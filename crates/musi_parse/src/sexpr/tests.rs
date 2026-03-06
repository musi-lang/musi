    use super::*;
    use crate::ast::{AstArenas, ParsedModule};
    use crate::parser::parse;
    use musi_shared::{DiagnosticBag, FileId, Interner, Span};

    /// Helper: lex + parse source text into a `ParsedModule`, returning the
    /// interner alongside for symbol resolution.
    fn parse_source(src: &str) -> (ParsedModule, Interner) {
        let mut interner = Interner::new();
        let mut diags = DiagnosticBag::new();
        let file_id = FileId(0);
        let lexed = musi_lex::lex(src, file_id, &mut interner, &mut diags);
        let module = parse(&lexed.tokens, file_id, &mut diags, &interner);
        (module, interner)
    }

    #[test]
    fn empty_module() {
        let (module, interner) = parse_source("");
        let out = dump(&module, &interner);
        assert_eq!(out, "(module)");
    }

    #[test]
    fn error_node_does_not_crash() {
        let mut ctx = AstArenas::new();
        let err = ctx.exprs.alloc(Expr::Error {
            span: Span::new(0, 1),
        });
        let module = ParsedModule {
            items: vec![err],
            ctx,
            span: Span::new(0, 1),
        };
        let interner = Interner::new();
        let out = dump(&module, &interner);
        assert!(out.contains("error"));
        assert!(!out.contains("Span"));
    }

    #[test]
    fn dump_expr_single_error() {
        let mut ctx = AstArenas::new();
        let err = ctx.exprs.alloc(Expr::Error {
            span: Span::new(0, 1),
        });
        let module = ParsedModule {
            items: vec![],
            ctx,
            span: Span::DUMMY,
        };
        let interner = Interner::new();
        let out = dump_expr(err, &module, &interner);
        assert_eq!(out, "error");
    }

    #[test]
    fn no_spans_in_output() {
        let mut interner = Interner::new();
        let name_sym = interner.intern("x");
        let mut ctx = AstArenas::new();
        let ident = ctx.exprs.alloc(Expr::Ident {
            name: name_sym,
            span: Span::new(42, 7),
        });
        let module = ParsedModule {
            items: vec![ident],
            ctx,
            span: Span::new(0, 50),
        };
        let out = dump(&module, &interner);
        assert!(out.contains("ident x"));
        assert!(!out.contains("42"));
        assert!(!out.contains("Span"));
    }

    #[test]
    fn binary_precedence_nesting() {
        // Build `1 + 2 * 3` manually: Binary(+, 1, Binary(*, 2, 3))
        let interner = Interner::new();
        let mut ctx = AstArenas::new();
        let one = ctx.exprs.alloc(Expr::Lit {
            value: LitValue::Int(1),
            span: Span::new(0, 1),
        });
        let two = ctx.exprs.alloc(Expr::Lit {
            value: LitValue::Int(2),
            span: Span::new(4, 1),
        });
        let three = ctx.exprs.alloc(Expr::Lit {
            value: LitValue::Int(3),
            span: Span::new(8, 1),
        });
        let mul = ctx.exprs.alloc(Expr::Binary {
            op: BinOp::Mul,
            lhs: two,
            rhs: three,
            span: Span::new(4, 5),
        });
        let add = ctx.exprs.alloc(Expr::Binary {
            op: BinOp::Add,
            lhs: one,
            rhs: mul,
            span: Span::new(0, 9),
        });
        let module = ParsedModule {
            items: vec![add],
            ctx,
            span: Span::new(0, 9),
        };
        let out = dump(&module, &interner);
        assert!(out.contains("(binary + (lit_int 1) (binary * (lit_int 2) (lit_int 3)))"));
    }

    #[test]
    fn lit_str_rendering() {
        let mut interner = Interner::new();
        let hello = interner.intern("Hello, world!");
        let mut ctx = AstArenas::new();
        let lit = ctx.exprs.alloc(Expr::Lit {
            value: LitValue::Str(hello),
            span: Span::new(0, 15),
        });
        let module = ParsedModule {
            items: vec![lit],
            ctx,
            span: Span::new(0, 15),
        };
        let out = dump(&module, &interner);
        assert!(out.contains("(lit_str \"Hello, world!\")"));
    }

    #[test]
    fn fn_def_rendering() {
        let mut interner = Interner::new();
        let add_sym = interner.intern("add");
        let a_sym = interner.intern("a");
        let b_sym = interner.intern("b");
        let int_sym = interner.intern("Int32");

        let mut ctx = AstArenas::new();

        let a_ident = ctx.exprs.alloc(Expr::Ident {
            name: a_sym,
            span: Span::DUMMY,
        });
        let b_ident = ctx.exprs.alloc(Expr::Ident {
            name: b_sym,
            span: Span::DUMMY,
        });
        let body = ctx.exprs.alloc(Expr::Binary {
            op: BinOp::Add,
            lhs: a_ident,
            rhs: b_ident,
            span: Span::DUMMY,
        });

        let int_ty = Ty::Named {
            name: int_sym,
            args: vec![],
            span: Span::DUMMY,
        };

        let fn_def = ctx.exprs.alloc(Expr::FnDef {
            attrs: vec![],
            modifiers: vec![],
            name: add_sym,
            ty_params: vec![],
            params: vec![
                Param {
                    attrs: vec![],
                    mutable: false,
                    name: a_sym,
                    ty: Some(int_ty.clone()),
                    span: Span::DUMMY,
                },
                Param {
                    attrs: vec![],
                    mutable: false,
                    name: b_sym,
                    ty: Some(int_ty.clone()),
                    span: Span::DUMMY,
                },
            ],
            ret_ty: Some(int_ty),
            where_clause: vec![],
            body: Some(body),
            span: Span::DUMMY,
        });

        let module = ParsedModule {
            items: vec![fn_def],
            ctx,
            span: Span::DUMMY,
        };
        let out = dump(&module, &interner);
        assert!(out.contains("fn_def"));
        assert!(out.contains("add"));
        assert!(out.contains("(param a Int32)"));
        assert!(out.contains("(param b Int32)"));
        assert!(out.contains("(ret Int32)"));
        assert!(out.contains("(binary + (ident a) (ident b))"));
    }

    #[test]
    fn unit_and_int_rendering() {
        let mut ctx = AstArenas::new();
        let unit = ctx.exprs.alloc(Expr::Unit { span: Span::DUMMY });
        let int_lit = ctx.exprs.alloc(Expr::Lit {
            value: LitValue::Int(42),
            span: Span::DUMMY,
        });
        let module = ParsedModule {
            items: vec![unit, int_lit],
            ctx,
            span: Span::DUMMY,
        };
        let interner = Interner::new();
        let out = dump(&module, &interner);
        assert!(out.contains("unit"));
        assert!(out.contains("(lit_int 42)"));
    }

    #[test]
    fn pattern_rendering() {
        let mut interner = Interner::new();
        let some_sym = interner.intern("Some");
        let x_sym = interner.intern("x");

        let pat = Pat::Ident {
            name: some_sym,
            suffix: Some(PatSuffix::Positional {
                args: vec![Pat::Ident {
                    name: x_sym,
                    suffix: None,
                    span: Span::DUMMY,
                }],
                span: Span::DUMMY,
            }),
            span: Span::DUMMY,
        };

        let mut ctx = AstArenas::new();
        let scrutinee = ctx.exprs.alloc(Expr::Ident {
            name: x_sym,
            span: Span::DUMMY,
        });
        let body = ctx.exprs.alloc(Expr::Unit { span: Span::DUMMY });
        let match_expr = ctx.exprs.alloc(Expr::Match {
            scrutinee,
            arms: vec![MatchArm {
                attrs: vec![],
                pat,
                guard: None,
                body,
                span: Span::DUMMY,
            }],
            span: Span::DUMMY,
        });

        let module = ParsedModule {
            items: vec![match_expr],
            ctx,
            span: Span::DUMMY,
        };
        let out = dump(&module, &interner);
        assert!(out.contains("(pat_sum Some [x])"));
    }

    #[test]
    fn type_rendering() {
        let mut interner = Interner::new();
        let opt_sym = interner.intern("Option");
        let int_sym = interner.intern("Int32");
        let a_sym = interner.intern("'a");

        let ty = Ty::Named {
            name: opt_sym,
            args: vec![Ty::Named {
                name: int_sym,
                args: vec![],
                span: Span::DUMMY,
            }],
            span: Span::DUMMY,
        };

        let mut ctx = AstArenas::new();
        let bind = ctx.exprs.alloc(Expr::Bind {
            attrs: vec![],
            modifiers: vec![],
            kind: BindKind::Const,
            pat: Pat::Ident {
                name: a_sym,
                suffix: None,
                span: Span::DUMMY,
            },
            ty: Some(ty),
            init: None,
            span: Span::DUMMY,
        });

        let module = ParsedModule {
            items: vec![bind],
            ctx,
            span: Span::DUMMY,
        };
        let out = dump(&module, &interner);
        assert!(out.contains("(apply Option [Int32])"));
    }
