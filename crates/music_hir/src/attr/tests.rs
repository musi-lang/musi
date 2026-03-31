use music_basic::Span;

use super::HirStringLit;

#[test]
fn test_string_lit_carries_span() {
    let lit = HirStringLit::new(Span::new(1, 4), None);
    assert_eq!(lit.span, Span::new(1, 4));
}
