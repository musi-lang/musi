use music_il::opcode::Opcode;

pub const PRELUDE_MODULE_NAME: &str = "musi:prelude";

/// A type class that is part of the Musi prelude.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PreludeClass {
    pub name: &'static str,
    pub methods: &'static [PreludeMethod],
}

/// Canonical Musi source for the compiler-owned injected prelude.
pub const PRELUDE_SOURCE: &str = include_str!("../prelude.ms");

/// A method within a prelude class, used for name resolution seeding.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PreludeMethod {
    pub name: &'static str,
    pub source_name: &'static str,
    pub op_name: &'static str,
    pub opcode: Option<Opcode>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BuiltinVariant {
    pub parent_type: &'static str,
    pub name: &'static str,
    pub opcode: Opcode,
    pub tag_index: u16,
    pub arity: u8,
}

/// All prelude type classes and their method names.
pub const PRELUDE_CLASSES: &[PreludeClass] = &[
    PreludeClass {
        name: "Eq",
        methods: &[
            PreludeMethod {
                name: "eq",
                source_name: "(=)",
                op_name: "(=)",
                opcode: Some(Opcode::CmpEq),
            },
            PreludeMethod {
                name: "ne",
                source_name: "(/=)",
                op_name: "(/=)",
                opcode: None,
            },
        ],
    },
    PreludeClass {
        name: "Ord",
        methods: &[
            PreludeMethod {
                name: "lt",
                source_name: "(<)",
                op_name: "(<)",
                opcode: Some(Opcode::CmpLt),
            },
            PreludeMethod {
                name: "gt",
                source_name: "(>)",
                op_name: "(>)",
                opcode: Some(Opcode::CmpGt),
            },
            PreludeMethod {
                name: "le",
                source_name: "(<=)",
                op_name: "(<=)",
                opcode: Some(Opcode::CmpLeq),
            },
            PreludeMethod {
                name: "ge",
                source_name: "(>=)",
                op_name: "(>=)",
                opcode: Some(Opcode::CmpGeq),
            },
        ],
    },
    PreludeClass {
        name: "Num",
        methods: &[
            PreludeMethod {
                name: "add",
                source_name: "(+)",
                op_name: "(+)",
                opcode: Some(Opcode::IAdd),
            },
            PreludeMethod {
                name: "sub",
                source_name: "(-)",
                op_name: "(-)",
                opcode: Some(Opcode::ISub),
            },
            PreludeMethod {
                name: "mul",
                source_name: "(*)",
                op_name: "(*)",
                opcode: Some(Opcode::IMul),
            },
            PreludeMethod {
                name: "div",
                source_name: "(/)",
                op_name: "(/)",
                opcode: Some(Opcode::IDiv),
            },
            PreludeMethod {
                name: "rem",
                source_name: "(%)",
                op_name: "(%)",
                opcode: Some(Opcode::IRem),
            },
            PreludeMethod {
                name: "neg",
                source_name: "(-)",
                op_name: "neg",
                opcode: Some(Opcode::INeg),
            },
        ],
    },
    PreludeClass {
        name: "Bits",
        methods: &[
            PreludeMethod {
                name: "shl",
                source_name: "(shl)",
                op_name: "shl",
                opcode: Some(Opcode::Shl),
            },
            PreludeMethod {
                name: "shr",
                source_name: "(shr)",
                op_name: "shr",
                opcode: Some(Opcode::Shr),
            },
        ],
    },
    PreludeClass {
        name: "Show",
        methods: &[PreludeMethod {
            name: "show",
            source_name: "show",
            op_name: "show",
            opcode: None,
        }],
    },
];

pub const BUILTIN_VARIANTS: &[BuiltinVariant] = &[
    BuiltinVariant {
        parent_type: "Bool",
        name: "False",
        opcode: Opcode::LdFls,
        tag_index: 0,
        arity: 0,
    },
    BuiltinVariant {
        parent_type: "Bool",
        name: "True",
        opcode: Opcode::LdTru,
        tag_index: 1,
        arity: 0,
    },
];

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
