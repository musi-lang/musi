use music_lex::TokenKind;

use crate::kinds::SyntaxNodeKind;
use crate::red::SyntaxNode;

macro_rules! support_wrapper {
    ($name:ident, $kind:ident) => {
        #[derive(Debug, Clone, Copy)]
        pub struct $name<'tree> {
            syntax: SyntaxNode<'tree>,
        }

        impl<'tree> $name<'tree> {
            #[must_use]
            pub fn cast(node: SyntaxNode<'tree>) -> Option<Self> {
                (node.kind() == SyntaxNodeKind::$kind).then_some(Self { syntax: node })
            }

            #[must_use]
            pub const fn syntax(self) -> SyntaxNode<'tree> {
                self.syntax
            }
        }
    };
}

support_wrapper!(Attr, Attr);
support_wrapper!(Param, Param);
support_wrapper!(Field, Field);
support_wrapper!(Variant, Variant);
support_wrapper!(TypeParam, TypeParam);
support_wrapper!(Constraint, Constraint);
support_wrapper!(HandlerClause, HandlerClause);
support_wrapper!(Member, Member);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemberKind {
    Let,
    Law,
}

impl Member<'_> {
    #[must_use]
    pub fn kind(self) -> Option<MemberKind> {
        self.syntax
            .child_tokens()
            .find_map(|token| match token.kind() {
                TokenKind::KwLet => Some(MemberKind::Let),
                TokenKind::KwLaw => Some(MemberKind::Law),
                _ => None,
            })
    }
}
