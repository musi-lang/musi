let sys := import "@std/sys";

--- Operating system name.
export opaque let Os := data {
  | Os(value : String)
};

--- CPU architecture name.
export opaque let Arch := data {
  | Arch(value : String)
};

--- Platform family name.
export opaque let Family := data {
  | Family(value : String)
};

--- Byte order.
export opaque let Endian := data {
  | Little
  | Big
};

--- JIT backend availability.
export opaque let Jit := data {
  | Supported(backend : String, isa : String)
  | Unsupported
};

--- Current compilation/runtime target facts.
export opaque let Target := data {
  | Target(os : Os, arch : Arch, family : Family, pointerWidth : Int,
    endian : Endian, jit : Jit
  )
};

--- Internal implementation for `os`.
export let os (value : String) : Os := .Os(value := value);
--- Internal implementation for `arch`.
export let arch (value : String) : Arch := .Arch(value := value);
--- Internal implementation for `family`.
export let family (value : String) : Family := .Family(value := value);

--- Internal implementation for `osText`.
export let osText (target : Os) : String := match target (
  | .Os(value := value) => value
);

--- Internal implementation for `archText`.
export let archText (target : Arch) : String := match target (
  | .Arch(value := value) => value
);

--- Internal implementation for `familyText`.
export let familyText (target : Family) : String := match target (
  | .Family(value := value) => value
);

--- Internal implementation for `endianFromString`.
export let endianFromString (value : String) : Endian := match value (
  | "big" =>.Big
  | _ =>.Little
);

--- Internal implementation for `isSupported`.
export let isSupported (target : Jit) : Bool := match target (
  | .Supported(backend := _, isa := _) => 0 = 0
  | .Unsupported => 0 = 1
);

--- Internal implementation for `currentJit`.
export let currentJit : Jit := match sys.jit.supported (
  | supported if supported =>.Supported(backend := sys.jit.backend,
    isa := sys.jit.isa
  )
  | _ =>.Unsupported
);

--- Internal implementation for `current`.
export let current : Target := .Target(os := os(sys.os), arch := arch(sys.arch),
  family := family(sys.family), pointerWidth := sys.pointerWidth,
  endian := endianFromString(sys.endian), jit := currentJit
);

--- Internal implementation for `targetOs`.
export let targetOs (target : Target) : Os := match target (
  | .Target(os := value, arch := _, family := _, pointerWidth := _, endian := _,
    jit := _
  ) => value
);

--- Internal implementation for `targetArch`.
export let targetArch (target : Target) : Arch := match target (
  | .Target(os := _, arch := value, family := _, pointerWidth := _, endian := _,
    jit := _
  ) => value
);

--- Internal implementation for `targetFamily`.
export let targetFamily (target : Target) : Family := match target (
  | .Target(os := _, arch := _, family := value, pointerWidth := _, endian := _,
    jit := _
  ) => value
);

--- Internal implementation for `targetPointerWidth`.
export let targetPointerWidth (target : Target) : Int := match target (
  | .Target(os := _, arch := _, family := _, pointerWidth := value, endian := _,
    jit := _
  ) => value
);

--- Internal implementation for `targetEndian`.
export let targetEndian (target : Target) : Endian := match target (
  | .Target(os := _, arch := _, family := _, pointerWidth := _, endian := value,
    jit := _
  ) => value
);

--- Internal implementation for `targetJit`.
export let targetJit (target : Target) : Jit := match target (
  | .Target(os := _, arch := _, family := _, pointerWidth := _, endian := _,
    jit := value
  ) => value
);

--- Internal implementation for `matchesOs`.
export let matchesOs (value : Os) : Bool := sys.matchesOs(osText(value));
--- Internal implementation for `matchesArch`.
export let matchesArch (value : Arch) : Bool := sys.matchesArch(archText(value));
--- Internal implementation for `matchesFamily`.
export let matchesFamily (value : Family) : Bool := sys.matchesFamily(familyText(value));
--- Internal implementation for `isUnix`.
export let isUnix () : Bool := matchesFamily(family("unix"));
--- Internal implementation for `isDarwin`.
export let isDarwin () : Bool := matchesFamily(family("darwin"));
--- Internal implementation for `isWindows`.
export let isWindows () : Bool := matchesFamily(family("windows"));
--- Internal implementation for `isWebAssembly`.
export let isWebAssembly () : Bool := matchesFamily(family("webassembly"));
--- Internal implementation for `supportsJit`.
export let supportsJit () : Bool := isSupported(currentJit);
