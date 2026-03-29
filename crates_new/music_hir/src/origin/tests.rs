use music_basic::Span;

use super::HirOrigin;

#[test]
fn test_origin_dummy_is_span_dummy_and_no_syntax() {
    let origin = HirOrigin::dummy();
    assert_eq!(origin.span, Span::DUMMY);
    assert!(origin.syntax.is_none());
}
