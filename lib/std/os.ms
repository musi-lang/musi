--! Operating system module.

let sys := import "#sys";

--- Operating system name.
export hidden let Os := data {
  | OsName(value : String)
};

--- CPU architecture name.
export hidden let Arch := data {
  | ArchName(value : String)
};

--- Platform family name.
export hidden let Family := data {
  | FamilyName(value : String)
};

--- Byte order.
export hidden let Endian := data {
  | Little
  | Big
};

--- Current compilation/runtime target facts.
export hidden let Target := data {
  | TargetInfo(
    os : String,
    arch : String,
    family : String,
    pointerWidth : Int,
    endian : String,
  )
};

--- Function for `os`.
let makeOs (value : String) := .OsName(value := value);

--- Function for `arch`.
let makeArch (value : String) := .ArchName(value := value);

--- Build CPU architecture value.
export let arch (value : String) := makeArch(value);
--- Function for `family`.
let makeFamily (value : String) := .FamilyName(value := value);

--- Build platform family value.
export let family (value : String) := makeFamily(value);

--- Return operating system name text.
export let osText (target : Os) :=
  match target (
  | .OsName(value := value) => value
  );

--- Return CPU architecture name text.
export let archText (target : Arch) :=
  match target (
  | .ArchName(value := value) => value
  );

--- Return platform family name text.
export let familyText (target : Family) :=
  match target (
  | .FamilyName(value := value) => value
  );

--- Convert byte-order text into Endian.
export let endianFromString (value : String) :=
  if value = "big" then .Big else .Little;

--- Return current target facts from system bindings.
export let current : Target :=
  .TargetInfo(
    os := sys.os,
    arch := sys.arch,
    family := sys.family,
    pointerWidth := sys.pointerWidth,
    endian := sys.endian,
  );

--- Return target operating system.
export let targetOs (target : Target) :=
  match target (
  | .TargetInfo(
    os := value,
    arch := _,
    family := _,
    pointerWidth := _,
    endian := _,
  ) => makeOs(value)
  );

--- Return target CPU architecture.
export let targetArch (target : Target) :=
  match target (
  | .TargetInfo(
    os := _,
    arch := value,
    family := _,
    pointerWidth := _,
    endian := _,
  ) => makeArch(value)
  );

--- Return target platform family.
export let targetFamily (target : Target) :=
  match target (
  | .TargetInfo(
    os := _,
    arch := _,
    family := value,
    pointerWidth := _,
    endian := _,
  ) => makeFamily(value)
  );

--- Return target pointer width in bits.
export let targetPointerWidth (target : Target) :=
  match target (
  | .TargetInfo(
    os := _,
    arch := _,
    family := _,
    pointerWidth := value,
    endian := _,
  ) => value
  );

--- Return target byte order.
export let targetEndian (target : Target) :=
  match target (
  | .TargetInfo(
    os := _,
    arch := _,
    family := _,
    pointerWidth := _,
    endian := value,
  ) => endianFromString(value)
  );

--- Returns true when current OS matches value.
export let matchesOs (value : Os) := sys.matchesOs(osText(value));
--- Returns true when current architecture matches value.
export let matchesArch (value : Arch) := sys.matchesArch(archText(value));
--- Returns true when current family matches value.
export let matchesFamily (value : Family) :=
  sys.matchesFamily(familyText(value));
--- Returns true on Unix-family targets.
export let isUnix () := matchesFamily(makeFamily("unix"));
--- Returns true on Darwin-family targets.
export let isDarwin () := matchesFamily(makeFamily("darwin"));
--- Returns true on Windows-family targets.
export let isWindows () := matchesFamily(makeFamily("windows"));
--- Returns true on WebAssembly-family targets.
export let isWebAssembly () := matchesFamily(makeFamily("webassembly"));

--- Convert value to string.
export let (self : Os).toString () := osText(self);

--- Convert value to string.
export let (self : Arch).toString () := archText(self);

--- Convert value to string.
export let (self : Family).toString () := familyText(self);

--- Target architecture.
export let (self : Target).arch () := targetArch(self);

--- Target family.
export let (self : Target).family () := targetFamily(self);

--- Target pointer width.
export let (self : Target).pointerWidth () := targetPointerWidth(self);

--- Target endian value.
export let (self : Target).endian () := targetEndian(self);
