let sys := import "#sys";

--- Operating system name.
export opaque let Os := data {
  | OsName(value : String)
};

--- CPU architecture name.
export opaque let Arch := data {
  | ArchName(value : String)
};

--- Platform family name.
export opaque let Family := data {
  | FamilyName(value : String)
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
  | TargetInfo(
    os : String,
    arch : String,
    family : String,
    pointerWidth : Int,
    endian : String,
    jitSupported : Int,
    jitBackend : String,
    jitIsa : String,
  )
};

--- Core operation for `os`.
let makeOs (value : String) : Os := .OsName(value := value);

--- Core operation for `arch`.
let makeArch (value : String) : Arch := .ArchName(value := value);

--- Core operation for `arch`.
export let arch (value : String) : Arch := makeArch(value);
--- Core operation for `family`.
let makeFamily (value : String) : Family := .FamilyName(value := value);

--- Core operation for `family`.
export let family (value : String) : Family := makeFamily(value);

--- Core operation for `osText`.
export let osText (target : Os) : String :=
  match target (
  | .OsName(value := value) => value
  );

--- Core operation for `archText`.
export let archText (target : Arch) : String :=
  match target (
  | .ArchName(value := value) => value
  );

--- Core operation for `familyText`.
export let familyText (target : Family) : String :=
  match target (
  | .FamilyName(value := value) => value
  );

--- Core operation for `endianFromString`.
export let endianFromString (value : String) : Endian :=
  match value (
  | "big" =>.Big
  | _ =>.Little
  );

--- Core operation for `isSupported`.
let jitIsSupported (target : Jit) : Bool :=
  match target (
  | .Supported(backend := _, isa := _) => 0 = 0
  | .Unsupported => 0 = 1
  );

--- Core operation for `isSupported`.
export let isSupported (target : Jit) : Bool := jitIsSupported(target);

--- Core operation for `currentJit`.
export let currentJit : Jit :=
  match sys.jit.supported (
  | supported if supported =>.Supported(
    backend := sys.jit.backend,
    isa := sys.jit.isa,
  )
  | _ =>.Unsupported
  );

let jitSupportedFlag : Int :=
  match sys.jit.supported (
  | supported if supported => 1
  | _ => 0
  );

--- Core operation for `current`.
let jitSupportedFlag : Int :=
  match sys.jit.supported (
  | supported if supported => 1
  | _ => 0
  );

--- Core operation for `current`.
export let current : Target :=
  .TargetInfo(
    os := sys.os,
    arch := sys.arch,
    family := sys.family,
    pointerWidth := sys.pointerWidth,
    endian := sys.endian,
    jitSupported := jitSupportedFlag,
    jitBackend := sys.jit.backend,
    jitIsa := sys.jit.isa,
  );

--- Core operation for `targetOs`.
export let targetOs (target : Target) : Os :=
  match target (
  | .TargetInfo(
    os := value,
    arch := _,
    family := _,
    pointerWidth := _,
    endian := _,
    jitSupported := _,
    jitBackend := _,
    jitIsa := _,
  ) => makeOs(value)
  );

--- Core operation for `targetArch`.
export let targetArch (target : Target) : Arch :=
  match target (
  | .TargetInfo(
    os := _,
    arch := value,
    family := _,
    pointerWidth := _,
    endian := _,
    jitSupported := _,
    jitBackend := _,
    jitIsa := _,
  ) => makeArch(value)
  );

--- Core operation for `targetFamily`.
export let targetFamily (target : Target) : Family :=
  match target (
  | .TargetInfo(
    os := _,
    arch := _,
    family := value,
    pointerWidth := _,
    endian := _,
    jitSupported := _,
    jitBackend := _,
    jitIsa := _,
  ) => makeFamily(value)
  );

--- Core operation for `targetPointerWidth`.
export let targetPointerWidth (target : Target) : Int :=
  match target (
  | .TargetInfo(
    os := _,
    arch := _,
    family := _,
    pointerWidth := value,
    endian := _,
    jitSupported := _,
    jitBackend := _,
    jitIsa := _,
  ) => value
  );

--- Core operation for `targetEndian`.
export let targetEndian (target : Target) : Endian :=
  match target (
  | .TargetInfo(
    os := _,
    arch := _,
    family := _,
    pointerWidth := _,
    endian := value,
    jitSupported := _,
    jitBackend := _,
    jitIsa := _,
  ) => endianFromString(value)
  );

--- Core operation for `targetJit`.
export let targetJit (target : Target) : Jit :=
  match target (
  | .TargetInfo(
    os := _,
    arch := _,
    family := _,
    pointerWidth := _,
    endian := _,
    jitSupported := supported,
    jitBackend := backend,
    jitIsa := isa,
  ) => match supported (
  | ok if ok > 0 =>.Supported(backend := backend, isa := isa)
  | _ =>.Unsupported
  )
  );

--- Core operation for `matchesOs`.
export let matchesOs (value : Os) : Bool := sys.matchesOs(osText(value));
--- Core operation for `matchesArch`.
export let matchesArch (value : Arch) : Bool :=
  sys.matchesArch(archText(value));
--- Core operation for `matchesFamily`.
export let matchesFamily (value : Family) : Bool :=
  sys.matchesFamily(familyText(value));
--- Core operation for `isUnix`.
export let isUnix () : Bool := matchesFamily(makeFamily("unix"));
--- Core operation for `isDarwin`.
export let isDarwin () : Bool := matchesFamily(makeFamily("darwin"));
--- Core operation for `isWindows`.
export let isWindows () : Bool := matchesFamily(makeFamily("windows"));
--- Core operation for `isWebAssembly`.
export let isWebAssembly () : Bool := matchesFamily(makeFamily("webassembly"));
--- Core operation for `supportsJit`.
export let supportsJit () : Bool := jitIsSupported(currentJit);

--- Convert value to string.
export let (self : Os).toString () : String := osText(self);

--- Convert value to string.
export let (self : Arch).toString () : String := archText(self);

--- Convert value to string.
export let (self : Family).toString () : String := familyText(self);

--- Returns true when JIT support exists.
export let (self : Jit).isSupported () : Bool := jitIsSupported(self);

--- Target architecture.
export let (self : Target).arch () : Arch := targetArch(self);

--- Target family.
export let (self : Target).family () : Family := targetFamily(self);

--- Target pointer width.
export let (self : Target).pointerWidth () : Int := targetPointerWidth(self);

--- Target endian value.
export let (self : Target).endian () : Endian := targetEndian(self);

--- Target JIT facts.
export let (self : Target).jit () : Jit := targetJit(self);
