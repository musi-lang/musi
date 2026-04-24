use super::*;

fn options(width: usize) -> PrettyOptions {
    PrettyOptions {
        line_width: width,
        indent: "  ".to_owned(),
    }
}

mod success {
    use super::*;

    #[test]
    fn group_renders_flat_when_it_fits() {
        let doc = Doc::group(Doc::concat(vec![
            Doc::text("call("),
            Doc::indent(Doc::concat(vec![Doc::soft_line(), Doc::text("x")])),
            Doc::soft_line(),
            Doc::text(")"),
        ]));

        assert_eq!(render(&doc, &options(80)), "call(x)");
    }

    #[test]
    fn group_breaks_when_it_does_not_fit() {
        let doc = Doc::group(Doc::concat(vec![
            Doc::text("call("),
            Doc::indent(Doc::concat(vec![Doc::line(), Doc::text("long_value")])),
            Doc::line(),
            Doc::text(")"),
        ]));

        assert_eq!(render(&doc, &options(8)), "call(\n  long_value\n)");
    }

    #[test]
    fn if_break_selects_mode_specific_doc() {
        let doc = Doc::group(Doc::concat(vec![
            Doc::text("["),
            Doc::indent(Doc::concat(vec![
                Doc::line(),
                Doc::text("long_value"),
                Doc::if_break(Doc::text(","), Doc::nil()),
            ])),
            Doc::line(),
            Doc::text("]"),
        ]));

        assert_eq!(render(&doc, &options(8)), "[\n  long_value,\n]");
    }

    #[test]
    fn join_inserts_separator_between_docs() {
        let doc = Doc::join(
            [Doc::text("a"), Doc::text("b"), Doc::text("c")],
            &Doc::text(", "),
        );

        assert_eq!(render(&doc, &options(80)), "a, b, c");
    }

    #[test]
    fn join_hardline_uses_newlines_between_docs() {
        let doc = Doc::join_hardline([Doc::text("alpha"), Doc::text("beta")]);

        assert_eq!(render(&doc, &options(80)), "alpha\nbeta");
    }

    #[test]
    fn render_with_final_newline_appends_single_newline() {
        let doc = Doc::text("value");

        assert_eq!(render_with_final_newline(&doc, &options(80)), "value\n");
    }

    #[test]
    fn render_with_final_newline_does_not_duplicate_existing_newline() {
        let doc = Doc::concat(vec![Doc::text("value"), Doc::hardline()]);

        assert_eq!(render_with_final_newline(&doc, &options(80)), "value\n");
    }
}

mod failure {}
