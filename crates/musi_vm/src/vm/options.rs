#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VmOptions {
    pub heap_limit_bytes: Option<usize>,
    pub max_object_bytes: Option<usize>,
    pub stack_frame_limit: Option<usize>,
    pub instruction_budget: Option<u64>,
    pub gc_stress: bool,
    pub optimization_level: VmOptimizationLevel,
}

impl Default for VmOptions {
    fn default() -> Self {
        Self::DEFAULT
    }
}

impl VmOptions {
    pub const DEFAULT: Self = Self {
        heap_limit_bytes: None,
        max_object_bytes: None,
        stack_frame_limit: None,
        instruction_budget: None,
        gc_stress: false,
        optimization_level: VmOptimizationLevel::Tiered,
    };

    #[must_use]
    pub const fn with_heap_limit_bytes(mut self, heap_limit_bytes: usize) -> Self {
        self.heap_limit_bytes = Some(heap_limit_bytes);
        self
    }

    #[must_use]
    pub const fn with_max_object_bytes(mut self, max_object_bytes: usize) -> Self {
        self.max_object_bytes = Some(max_object_bytes);
        self
    }

    #[must_use]
    pub const fn with_stack_frame_limit(mut self, stack_frame_limit: usize) -> Self {
        self.stack_frame_limit = Some(stack_frame_limit);
        self
    }

    #[must_use]
    pub const fn with_instruction_budget(mut self, instruction_budget: u64) -> Self {
        self.instruction_budget = Some(instruction_budget);
        self
    }

    #[must_use]
    pub const fn with_gc_stress(mut self, gc_stress: bool) -> Self {
        self.gc_stress = gc_stress;
        self
    }

    #[must_use]
    pub const fn with_optimization_level(
        mut self,
        optimization_level: VmOptimizationLevel,
    ) -> Self {
        self.optimization_level = optimization_level;
        self
    }
}

#[allow(non_upper_case_globals)]
pub const VmOptions: VmOptions = VmOptions::DEFAULT;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum VmOptimizationLevel {
    Interpreter,
    #[default]
    Tiered,
}

pub(super) const DEFAULT_AUTO_COLLECT_THRESHOLD_BYTES: usize = 1024 * 1024;
