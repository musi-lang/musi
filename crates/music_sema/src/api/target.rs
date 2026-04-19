use std::collections::BTreeSet;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ForeignLinkInfo {
    pub name: Option<Box<str>>,
    pub symbol: Option<Box<str>>,
}

impl ForeignLinkInfo {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            name: None,
            symbol: None,
        }
    }

    #[must_use]
    pub fn with_name<Name>(mut self, name: Name) -> Self
    where
        Name: Into<Box<str>>,
    {
        self.name = Some(name.into());
        self
    }

    #[must_use]
    pub fn with_symbol<SymbolName>(mut self, symbol: SymbolName) -> Self
    where
        SymbolName: Into<Box<str>>,
    {
        self.symbol = Some(symbol.into());
        self
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TargetInfo {
    pub triple: Option<Box<str>>,
    pub os: Option<Box<str>>,
    pub arch: Option<Box<str>>,
    pub arch_family: Option<Box<str>>,
    pub env: Option<Box<str>>,
    pub abi: Option<Box<str>>,
    pub vendor: Option<Box<str>>,
    pub family: BTreeSet<Box<str>>,
    pub features: BTreeSet<Box<str>>,
    pub pointer_width: Option<u16>,
    pub endian: Option<Box<str>>,
    pub jit: JitTargetInfo,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct JitTargetInfo {
    pub supported: bool,
    pub backend: Option<Box<str>>,
    pub isa: Option<Box<str>>,
    pub call_conv: Option<Box<str>>,
    pub features: BTreeSet<Box<str>>,
}

impl TargetInfo {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            triple: None,
            os: None,
            arch: None,
            arch_family: None,
            env: None,
            abi: None,
            vendor: None,
            family: BTreeSet::new(),
            features: BTreeSet::new(),
            pointer_width: None,
            endian: None,
            jit: JitTargetInfo {
                supported: false,
                backend: None,
                isa: None,
                call_conv: None,
                features: BTreeSet::new(),
            },
        }
    }

    #[must_use]
    pub fn with_triple<Triple>(mut self, triple: Triple) -> Self
    where
        Triple: Into<Box<str>>,
    {
        self.triple = Some(normalize_target_text(&triple.into()).into_boxed_str());
        self
    }

    #[must_use]
    pub fn with_os<Os>(mut self, os: Os) -> Self
    where
        Os: Into<Box<str>>,
    {
        self.os = Some(normalize_target_text(&os.into()).into_boxed_str());
        self
    }

    #[must_use]
    pub fn with_arch<Arch>(mut self, arch: Arch) -> Self
    where
        Arch: Into<Box<str>>,
    {
        let arch = normalize_arch_text(&arch.into());
        self.arch_family = arch_family(&arch).map(Into::into);
        self.arch = Some(arch.into_boxed_str());
        self.jit = jit_target_for_arch(self.arch.as_deref());
        self
    }

    #[must_use]
    pub fn with_arch_family<ArchFamily>(mut self, arch_family: ArchFamily) -> Self
    where
        ArchFamily: Into<Box<str>>,
    {
        self.arch_family = Some(normalize_target_text(&arch_family.into()).into_boxed_str());
        self
    }

    #[must_use]
    pub fn with_env<Env>(mut self, env: Env) -> Self
    where
        Env: Into<Box<str>>,
    {
        self.env = Some(normalize_target_text(&env.into()).into_boxed_str());
        self
    }

    #[must_use]
    pub fn with_abi<Abi>(mut self, abi: Abi) -> Self
    where
        Abi: Into<Box<str>>,
    {
        self.abi = Some(normalize_target_text(&abi.into()).into_boxed_str());
        self
    }

    #[must_use]
    pub fn with_vendor<Vendor>(mut self, vendor: Vendor) -> Self
    where
        Vendor: Into<Box<str>>,
    {
        self.vendor = Some(normalize_target_text(&vendor.into()).into_boxed_str());
        self
    }

    #[must_use]
    pub fn with_features(mut self, features: BTreeSet<Box<str>>) -> Self {
        self.features = features
            .into_iter()
            .map(|feature| normalize_target_text(&feature).into_boxed_str())
            .collect();
        self
    }

    #[must_use]
    pub fn with_family<Family>(mut self, family: Family) -> Self
    where
        Family: Into<Box<str>>,
    {
        let _ = self
            .family
            .insert(normalize_target_text(&family.into()).into_boxed_str());
        self
    }

    #[must_use]
    pub const fn with_pointer_width(mut self, pointer_width: u16) -> Self {
        self.pointer_width = Some(pointer_width);
        self
    }

    #[must_use]
    pub fn with_endian<Endian>(mut self, endian: Endian) -> Self
    where
        Endian: Into<Box<str>>,
    {
        self.endian = Some(normalize_target_text(&endian.into()).into_boxed_str());
        self
    }
}

#[must_use]
pub fn normalize_target_text(text: &str) -> String {
    text.trim().to_ascii_lowercase().replace('_', "-")
}

#[must_use]
pub fn normalize_arch_text(text: &str) -> String {
    match normalize_target_text(text).as_str() {
        "x86-64" => "x86-64".into(),
        "aarch64" => "aarch64".into(),
        "arm" => "aarch32".into(),
        other => other.into(),
    }
}

#[must_use]
pub fn arch_family(arch: &str) -> Option<&'static str> {
    match normalize_arch_text(arch).as_str() {
        "x86" | "x86-64" => Some("x86"),
        "aarch32" | "aarch64" => Some("arm"),
        "rv32" | "rv64" | "riscv32" | "riscv64" => Some("risc-v"),
        "wasm32" | "wasm64" => Some("webassembly"),
        "powerpc" | "powerpc64" => Some("powerpc"),
        "mips" | "mips64" => Some("mips"),
        "loongarch32" | "loongarch64" => Some("loongarch"),
        "s390x" => Some("ibm-z"),
        _ => None,
    }
}

#[must_use]
pub fn jit_target_for_arch(arch: Option<&str>) -> JitTargetInfo {
    let Some(arch) = arch else {
        return JitTargetInfo::default();
    };
    let isa = match normalize_arch_text(arch).as_str() {
        "x86-64" => "x64",
        "aarch64" => "aarch64",
        "rv64" | "riscv64" => "riscv64",
        "s390x" => "s390x",
        _ => return JitTargetInfo::default(),
    };
    JitTargetInfo {
        supported: true,
        backend: Some("cranelift".into()),
        isa: Some(isa.into()),
        call_conv: None,
        features: BTreeSet::new(),
    }
}
