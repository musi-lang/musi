--! System module.

@foreign(abi := .musi)
let Musi__targetOs () : String;

@foreign(abi := .musi)
let Musi__targetArch () : String;

@foreign(abi := .musi)
let Musi__targetArchFamily () : String;

@foreign(abi := .musi)
let Musi__targetFamily () : String;

@foreign(abi := .musi)
let Musi__targetPointerWidth () : Int;

@foreign(abi := .musi)
let Musi__targetEndian () : String;

@foreign(abi := .musi)
let Musi__matchesOs (value : String) : Bit;

@foreign(abi := .musi)
let Musi__matchesArch (value : String) : Bit;

@foreign(abi := .musi)
let Musi__matchesFamily (value : String) : Bit;

--- Raw target operating system name.
export let os := unsafe (Musi__targetOs());
--- Raw target architecture name.
export let arch = unsafe (Musi__targetArch());
--- Raw target architecture family name.
export let archFamily := unsafe (Musi__targetArchFamily());
--- Raw target platform family name.
export let family := unsafe (Musi__targetFamily());
--- Target pointer width in bits.
export let pointerWidth := unsafe (Musi__targetPointerWidth());
--- Target byte order name.
export let endian := unsafe (Musi__targetEndian());

--- Raw target fact record.
export let target :=
  {
    os := os,
    arch := arch,
    archFamily := archFamily,
    family := family,
    pointerWidth := pointerWidth,
    endian := endian,
  };

--- Returns true when value names current OS.
export let matchesOs (value : String) := unsafe (Musi__matchesOs(value));
--- Returns true when value names current architecture.
export let matchesArch (value : String) := unsafe (Musi__matchesArch(value));
--- Returns true when value names current platform family.
export let matchesFamily (value : String) :=
  unsafe (Musi__matchesFamily(value));

--- Returns true on Unix-family targets.
export let isUnix () := matchesFamily("unix");
--- Returns true on Darwin-family targets.
export let isDarwin () := matchesFamily("darwin");
--- Returns true on Windows targets.
export let isWindows () := matchesFamily("windows");
--- Returns true on WebAssembly-family targets.
export let isWebAssembly () := matchesFamily("webassembly");
