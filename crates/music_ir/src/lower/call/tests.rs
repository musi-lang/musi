use music_sema::ExprMemberKind;

use super::is_dot_callable_member;

mod success {}

mod failure {
    use super::*;

    #[test]
    fn dot_callable_member_rejects_other_member_kinds() {
        assert!(!is_dot_callable_member(ExprMemberKind::RecordField));
        assert!(!is_dot_callable_member(ExprMemberKind::ClassMember));
        assert!(!is_dot_callable_member(ExprMemberKind::ModuleExport));
        assert!(!is_dot_callable_member(ExprMemberKind::FfiPointerExport));
        assert!(!is_dot_callable_member(ExprMemberKind::EffectOperation));
    }

    #[test]
    fn dot_callable_member_accepts_dot_callable_kind() {
        assert!(is_dot_callable_member(ExprMemberKind::DotCallable));
    }
}
