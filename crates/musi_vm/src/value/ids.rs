#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IsolateId(u64);

impl IsolateId {
    #[must_use]
    pub(crate) const fn new(raw: u64) -> Self {
        Self(raw)
    }

    #[must_use]
    pub const fn raw(self) -> u64 {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GcRef {
    pub(crate) isolate: IsolateId,
    pub(crate) slot: usize,
    pub(crate) generation: u32,
}

impl GcRef {
    #[must_use]
    pub(crate) const fn new(isolate: IsolateId, slot: usize, generation: u32) -> Self {
        Self {
            isolate,
            slot,
            generation,
        }
    }

    #[must_use]
    pub const fn isolate(self) -> IsolateId {
        self.isolate
    }

    #[must_use]
    pub const fn slot(self) -> usize {
        self.slot
    }

    #[must_use]
    pub const fn generation(self) -> u32 {
        self.generation
    }
}
