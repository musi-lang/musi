let core := import "./core.ms";

--- Operating system name.
export let Os := core.Os;

--- CPU architecture name.
export let Arch := core.Arch;

--- Platform family name.
export let Family := core.Family;

--- Byte order.
export let Endian := core.Endian;

--- JIT backend availability.
export let Jit := core.Jit;

--- Current compilation/runtime target facts.
export let Target := core.Target;

--- Build an OS name.
export let os := core.os;

--- Build an architecture name.
export let arch := core.arch;

--- Build a platform family name.
export let family := core.family;

--- Current JIT support facts.
export let currentJit := core.currentJit;

--- Current target facts.
export let current := core.current;

--- Convert raw endian text to typed endian value.
export let endianFromString := core.endianFromString;

--- Match current OS.
export let matchesOs := core.matchesOs;

--- Match current architecture.
export let matchesArch := core.matchesArch;

--- Match current platform family.
export let matchesFamily := core.matchesFamily;

--- Returns true on Unix-family platforms.
export let isUnix := core.isUnix;

--- Returns true on Darwin-family platforms.
export let isDarwin := core.isDarwin;

--- Returns true on Windows.
export let isWindows := core.isWindows;

--- Returns true on WebAssembly.
export let isWebAssembly := core.isWebAssembly;

--- Returns true when current target can use the planned JIT backend.
export let supportsJit := core.supportsJit;

--- Convert value to string.
export let(self : Os).toString () : String := core.osText(self);

--- Convert value to string.
export let(self : Arch).toString () : String := core.archText(self);

--- Convert value to string.
export let(self : Family).toString () : String := core.familyText(self);

--- Returns true when JIT support exists.
export let(self : Jit).isSupported () : Bool := core.isSupported(self);

--- Target OS.
export let(self : Target).os () : Os := core.targetOs(self);

--- Target architecture.
export let(self : Target).arch () : Arch := core.targetArch(self);

--- Target family.
export let(self : Target).family () : Family := core.targetFamily(self);

--- Target pointer width.
export let(self : Target).pointerWidth () : Int :=
    core.targetPointerWidth(self);

--- Target endian value.
export let(self : Target).endian () : Endian := core.targetEndian(self);

--- Target JIT facts.
export let(self : Target).jit () : Jit := core.targetJit(self);

--- Target OS.
export let targetOs := core.targetOs;

--- Target architecture.
export let targetArch := core.targetArch;

--- Target family.
export let targetFamily := core.targetFamily;

--- Target pointer width.
export let targetPointerWidth := core.targetPointerWidth;

--- Target endian value.
export let targetEndian := core.targetEndian;

--- Target JIT facts.
export let targetJit := core.targetJit;
