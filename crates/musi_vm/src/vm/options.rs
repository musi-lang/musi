use std::error::Error;
use std::fmt::{Display, Formatter, Result as FmtResult};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VmOptions {
    pub heap_limit_bytes: Option<usize>,
    pub max_object_bytes: Option<usize>,
    pub stack_frame_limit: Option<usize>,
    pub instruction_budget: Option<u64>,
    pub gc_stress: bool,
    pub mode: MvmMode,
    pub features: MvmFeatures,
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
        mode: MvmMode::Normal,
        features: MvmFeatures::DEFAULT,
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
        self.mode = optimization_level;
        self
    }

    #[must_use]
    pub const fn with_mode(mut self, mode: MvmMode) -> Self {
        self.mode = mode;
        self
    }

    #[must_use]
    pub const fn with_quickening(mut self, quickening: bool) -> Self {
        self.features = self.features.with_quickening(quickening);
        self
    }

    #[must_use]
    pub const fn with_runtime_kernels(mut self, runtime_kernels: bool) -> Self {
        self.features = self.features.with_runtime_kernels(runtime_kernels);
        self
    }

    #[must_use]
    pub const fn with_fused_dispatch(mut self, fused_dispatch: bool) -> Self {
        self.features = self.features.with_fused_dispatch(fused_dispatch);
        self
    }

    #[must_use]
    pub const fn with_inline_caches(mut self, inline_caches: bool) -> Self {
        self.features = self.features.with_inline_caches(inline_caches);
        self
    }
}

#[allow(non_upper_case_globals)]
pub const VmOptions: VmOptions = VmOptions::DEFAULT;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum MvmMode {
    DebugInterpreter,
    Interpreter,
    #[default]
    Normal,
    Hot,
}

impl MvmMode {
    #[allow(non_upper_case_globals)]
    pub const Tiered: Self = Self::Normal;
}

pub type VmOptimizationLevel = MvmMode;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MvmFeatures {
    bits: u8,
}

impl Default for MvmFeatures {
    fn default() -> Self {
        Self::DEFAULT
    }
}

impl MvmFeatures {
    const QUICKENING: u8 = 0b0001;
    const RUNTIME_KERNELS: u8 = 0b0010;
    const FUSED_DISPATCH: u8 = 0b0100;
    const INLINE_CACHES: u8 = 0b1000;

    pub const DEFAULT: Self = Self {
        bits: Self::QUICKENING | Self::RUNTIME_KERNELS | Self::FUSED_DISPATCH | Self::INLINE_CACHES,
    };

    #[must_use]
    pub const fn has_quickening(self) -> bool {
        self.has(Self::QUICKENING)
    }

    #[must_use]
    pub const fn has_runtime_kernels(self) -> bool {
        self.has(Self::RUNTIME_KERNELS)
    }

    #[must_use]
    pub const fn has_fused_dispatch(self) -> bool {
        self.has(Self::FUSED_DISPATCH)
    }

    #[must_use]
    pub const fn has_inline_caches(self) -> bool {
        self.has(Self::INLINE_CACHES)
    }

    #[must_use]
    pub const fn with_quickening(self, enabled: bool) -> Self {
        self.with(Self::QUICKENING, enabled)
    }

    #[must_use]
    pub const fn with_runtime_kernels(self, enabled: bool) -> Self {
        self.with(Self::RUNTIME_KERNELS, enabled)
    }

    #[must_use]
    pub const fn with_fused_dispatch(self, enabled: bool) -> Self {
        self.with(Self::FUSED_DISPATCH, enabled)
    }

    #[must_use]
    pub const fn with_inline_caches(self, enabled: bool) -> Self {
        self.with(Self::INLINE_CACHES, enabled)
    }

    const fn has(self, bit: u8) -> bool {
        self.bits & bit != 0
    }

    const fn with(self, bit: u8, enabled: bool) -> Self {
        if enabled {
            Self {
                bits: self.bits | bit,
            }
        } else {
            Self {
                bits: self.bits & !bit,
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MvmOptionsParseError {
    message: Box<str>,
}

impl MvmOptionsParseError {
    #[must_use]
    pub fn message(&self) -> &str {
        &self.message
    }
}

impl Display for MvmOptionsParseError {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> FmtResult {
        formatter.write_str(&self.message)
    }
}

impl Error for MvmOptionsParseError {}

impl VmOptions {
    /// Parses MVM options from `MVM_OPTIONS` and `-Xmvm:` CLI arguments.
    ///
    /// # Errors
    ///
    /// Returns an error when an option is unknown, has an invalid value, or
    /// does not use the `-Xmvm:` prefix.
    pub fn parse_mvm_options(
        env_options: Option<&str>,
        args: &[String],
    ) -> Result<Self, MvmOptionsParseError> {
        let mut options = Self::DEFAULT;
        if let Some(env_options) = env_options {
            for option in env_options.split_whitespace() {
                options = parse_mvm_option(options, option)?;
            }
        }
        for option in args {
            options = parse_mvm_option(options, option)?;
        }
        Ok(options)
    }
}

pub(super) const DEFAULT_AUTO_COLLECT_THRESHOLD_BYTES: usize = 1024 * 1024;

fn parse_mvm_option(
    mut options: VmOptions,
    option: &str,
) -> Result<VmOptions, MvmOptionsParseError> {
    let Some(raw) = option.strip_prefix("-Xmvm:") else {
        return Err(parse_error(format!(
            "MVM option `{option}` must start with `-Xmvm:`"
        )));
    };
    if let Some(flag) = raw.strip_prefix('+') {
        return match flag {
            "UseQuickening" => Ok(options.with_quickening(true)),
            "UseKernels" => Ok(options.with_runtime_kernels(true)),
            "UseFusedDispatch" => Ok(options.with_fused_dispatch(true)),
            "UseInlineCaches" => Ok(options.with_inline_caches(true)),
            _ => Err(parse_error(format!("unknown MVM option `{option}`"))),
        };
    }
    if let Some(flag) = raw.strip_prefix('-') {
        return match flag {
            "UseQuickening" => Ok(options.with_quickening(false)),
            "UseKernels" => Ok(options.with_runtime_kernels(false)),
            "UseFusedDispatch" => Ok(options.with_fused_dispatch(false)),
            "UseInlineCaches" => Ok(options.with_inline_caches(false)),
            _ => Err(parse_error(format!("unknown MVM option `{option}`"))),
        };
    }
    let Some((key, value)) = raw.split_once('=') else {
        return Err(parse_error(format!(
            "MVM option `{option}` needs `key=value`"
        )));
    };
    match key {
        "Tier" => {
            options.mode = match value {
                "Normal" => MvmMode::Normal,
                "Interp" => MvmMode::Interpreter,
                "Debug" => MvmMode::DebugInterpreter,
                "Hot" => MvmMode::Hot,
                _ => return Err(parse_error(format!("unknown MVM mode `{value}`"))),
            };
            Ok(options)
        }
        "HeapLimit" => parse_usize(value, option).map(|value| options.with_heap_limit_bytes(value)),
        "StackLimit" => {
            parse_usize(value, option).map(|value| options.with_stack_frame_limit(value))
        }
        "StepLimit" => {
            let value = value.parse::<u64>().map_err(|_| {
                parse_error(format!("MVM option `{option}` expects unsigned integer"))
            })?;
            Ok(options.with_instruction_budget(value))
        }
        "GC" => match value {
            "Auto" => Ok(options.with_gc_stress(false)),
            "Stress" => Ok(options.with_gc_stress(true)),
            _ => Err(parse_error(format!("unknown MVM GC mode `{value}`"))),
        },
        _ => Err(parse_error(format!("unknown MVM option `{option}`"))),
    }
}

fn parse_usize(value: &str, option: &str) -> Result<usize, MvmOptionsParseError> {
    value
        .parse::<usize>()
        .map_err(|_| parse_error(format!("MVM option `{option}` expects unsigned integer")))
}

fn parse_error(message: String) -> MvmOptionsParseError {
    MvmOptionsParseError {
        message: message.into_boxed_str(),
    }
}
