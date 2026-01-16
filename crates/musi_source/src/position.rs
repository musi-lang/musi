#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct SourcePosition {
    /// Byte position
    pub position: u32,
}

impl SourcePosition {
    /// Create new source position from byte position
    #[must_use]
    pub const fn new(position: u32) -> Self {
        Self { position }
    }
}

#[cfg(test)]
mod tests;
