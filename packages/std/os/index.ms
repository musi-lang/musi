let sys := import "@std/sys";

/// Operating system name.
export opaque let Os := data {
  | Os(value : String)
};

/// CPU architecture name.
export opaque let Arch := data {
  | Arch(value : String)
};

/// Platform family name.
export opaque let Family := data {
  | Family(value : String)
};

/// Byte order.
export opaque let Endian := data {
  | Little
  | Big
};

/// JIT backend availability.
export opaque let Jit := data {
  | Supported(backend : String, isa : String)
  | Unsupported
};

/// Current compilation/runtime target facts.
export opaque let Target := data {
  | Target(
    os : Os,
    arch : Arch,
    family : Family,
    pointerWidth : Int,
    endian : Endian,
    jit : Jit
  )
};

/// Build an OS name.
export let os (value : String) : Os := .Os(value := value);

/// Build an architecture name.
export let arch (value : String) : Arch := .Arch(value := value);

/// Build a platform family name.
export let family (value : String) : Family := .Family(value := value);

/// Return OS name text.
let osText (target : Os) : String :=
  match target (
  | .Os(value := value) => value
  );

/// Convert value to string.
export let (self : Os).toString () : String := osText(self);

/// Return OS name text.
export let osName (value : Os) : String := osText(value);

/// Return architecture name text.
let archText (target : Arch) : String :=
  match target (
  | .Arch(value := value) => value
  );

/// Convert value to string.
export let (self : Arch).toString () : String := archText(self);

/// Return architecture name text.
export let archName (value : Arch) : String := archText(value);

/// Return family name text.
let familyText (target : Family) : String :=
  match target (
  | .Family(value := value) => value
  );

/// Convert value to string.
export let (self : Family).toString () : String := familyText(self);

/// Return family name text.
export let familyName (value : Family) : String := familyText(value);

/// Convert raw endian text to typed endian value.
export let endianFromString (value : String) : Endian :=
  match value (
  | "big" => .Big
  | _ => .Little
  );

/// Return true when JIT support exists.
export let (self : Jit).isSupported () : Bool :=
  match self (
  | .Supported(backend := _, isa := _) => 0 = 0
  | .Unsupported => 0 = 1
  );

/// Current JIT support facts.
export let currentJit : Jit :=
  match sys.jit.supported (
  | supported if supported => .Supported(backend := sys.jit.backend, isa := sys.jit.isa)
  | _ => .Unsupported
  );

/// Current target facts.
export let current : Target := .Target(
  os := os(sys.os),
  arch := arch(sys.arch),
  family := family(sys.family),
  pointerWidth := sys.pointerWidth,
  endian := endianFromString(sys.endian),
  jit := currentJit
);

/// Target OS.
export let targetOs (target : Target) : Os :=
  match target (
  | .Target(os := value, arch := _, family := _, pointerWidth := _, endian := _, jit := _) => value
  );

/// Target architecture.
export let targetArch (target : Target) : Arch :=
  match target (
  | .Target(os := _, arch := value, family := _, pointerWidth := _, endian := _, jit := _) => value
  );

/// Target family.
export let targetFamily (target : Target) : Family :=
  match target (
  | .Target(os := _, arch := _, family := value, pointerWidth := _, endian := _, jit := _) => value
  );

/// Target pointer width.
export let targetPointerWidth (target : Target) : Int :=
  match target (
  | .Target(os := _, arch := _, family := _, pointerWidth := value, endian := _, jit := _) => value
  );

/// Target endian value.
export let targetEndian (target : Target) : Endian :=
  match target (
  | .Target(os := _, arch := _, family := _, pointerWidth := _, endian := value, jit := _) => value
  );

/// Target JIT facts.
export let targetJit (target : Target) : Jit :=
  match target (
  | .Target(os := _, arch := _, family := _, pointerWidth := _, endian := _, jit := value) => value
  );

/// Match current OS.
export let matchesOs (value : Os) : Bool := sys.matchesOs(osText(value));

/// Match current architecture.
export let matchesArch (value : Arch) : Bool := sys.matchesArch(archText(value));

/// Match current platform family.
export let matchesFamily (value : Family) : Bool := sys.matchesFamily(familyText(value));

/// Returns true on Unix-family platforms.
export let isUnix () : Bool := matchesFamily(family("unix"));

/// Returns true on Darwin-family platforms.
export let isDarwin () : Bool := matchesFamily(family("darwin"));

/// Returns true on Windows.
export let isWindows () : Bool := matchesFamily(family("windows"));

/// Returns true on WebAssembly.
export let isWebAssembly () : Bool := matchesFamily(family("webassembly"));

/// Returns true when current target can use the planned JIT backend.
export let supportsJit () : Bool := isSupported(currentJit);
