use super::*;

impl Parser<'_> {
    pub(crate) fn node(
        &mut self,
        kind: SyntaxNodeKind,
        children: SyntaxElementList,
    ) -> SyntaxNodeId {
        self.builder.push_node_from_children(kind, children)
    }

    pub(crate) fn node1(&mut self, kind: SyntaxNodeKind, a: SyntaxElementId) -> SyntaxNodeId {
        self.node(kind, vec![a])
    }

    pub(crate) fn node2(
        &mut self,
        kind: SyntaxNodeKind,
        a: SyntaxElementId,
        b: SyntaxElementId,
    ) -> SyntaxNodeId {
        self.node(kind, vec![a, b])
    }

    pub(crate) fn node3(
        &mut self,
        kind: SyntaxNodeKind,
        a: SyntaxElementId,
        b: SyntaxElementId,
        c: SyntaxElementId,
    ) -> SyntaxNodeId {
        self.node(kind, vec![a, b, c])
    }

    pub(crate) fn wrap_list(
        &mut self,
        kind: SyntaxNodeKind,
        open: SyntaxElementId,
        mut inner: SyntaxElementList,
        close: SyntaxElementId,
    ) -> SyntaxNodeId {
        let mut children = vec![open];
        children.append(&mut inner);
        children.push(close);
        self.node(kind, children)
    }

    pub(crate) fn rewrap_node(
        &mut self,
        node: SyntaxNodeId,
        children: SyntaxElementList,
    ) -> SyntaxNodeId {
        let kind = self.builder.node_kind(node);
        self.node(kind, children)
    }
}
