use std::{hash, marker::PhantomData};

/// Typed identifier for arena-allocated nodes.
#[derive(Debug)]
pub struct NodeId<T> {
    index: u32,
    _marker: PhantomData<T>,
}

impl<T> NodeId<T> {
    /// Creates new node identifier.
    #[must_use]
    pub const fn new(index: u32) -> Self {
        Self {
            index,
            _marker: PhantomData,
        }
    }

    /// Returns raw index of node.
    #[must_use]
    pub const fn index(self) -> u32 {
        self.index
    }

    /// Converts index to `usize`.
    ///
    /// # Panics
    ///
    /// Panics if index does not fit in `usize` (only on 16-bit platforms).
    #[must_use]
    pub fn as_usize(self) -> usize {
        usize::try_from(self.index).expect("index too large")
    }
}

impl<T> Clone for NodeId<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for NodeId<T> {}

impl<T> PartialEq for NodeId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<T> Eq for NodeId<T> {}

impl<T> hash::Hash for NodeId<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}

impl<T> Default for NodeId<T> {
    fn default() -> Self {
        Self::new(0)
    }
}

/// Arena allocator for AST nodes.
#[derive(Debug)]
pub struct Arena<T> {
    nodes: Vec<T>,
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Arena<T> {
    /// Creates new empty arena.
    #[must_use]
    pub const fn new() -> Self {
        Self { nodes: vec![] }
    }

    /// Creates new arena with specified capacity.
    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            nodes: Vec::with_capacity(capacity),
        }
    }

    /// Allocates node and returns its unique ID.
    ///
    /// # Panics
    ///
    /// Panics if arena contains more than `u32::MAX` nodes.
    pub fn alloc(&mut self, node: T) -> NodeId<T> {
        let index = u32::try_from(self.nodes.len()).expect("arena overflow");
        self.nodes.push(node);
        NodeId::new(index)
    }

    /// Returns reference to node with given ID.
    ///
    /// # Panics
    ///
    /// Panics if ID is out of bounds.
    #[must_use]
    pub fn get(&self, id: NodeId<T>) -> &T {
        &self.nodes[id.as_usize()]
    }

    /// Returns mutable reference to node with given ID.
    ///
    /// # Panics
    ///
    /// Panics if ID is out of bounds.
    #[must_use]
    pub fn get_mut(&mut self, id: NodeId<T>) -> &mut T {
        &mut self.nodes[id.as_usize()]
    }

    /// Returns number of nodes in arena.
    #[must_use]
    pub const fn len(&self) -> usize {
        self.nodes.len()
    }

    /// Returns `true` if arena is empty.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    /// Iterates over all nodes with their IDs.
    ///
    /// # Panics
    ///
    /// Panics if arena contains more than `u32::MAX` nodes during iteration.
    pub fn iter(&self) -> impl Iterator<Item = (NodeId<T>, &T)> {
        self.nodes
            .iter()
            .enumerate()
            .map(|(i, n)| (NodeId::new(u32::try_from(i).expect("index overflow")), n))
    }
}
