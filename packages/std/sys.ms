@link(symbol := "sys.target.os")
native "musi" let targetOsIntrinsic () : String;

@link(symbol := "sys.target.arch")
native "musi" let targetArchIntrinsic () : String;

@link(symbol := "sys.target.arch_family")
native "musi" let targetArchFamilyIntrinsic () : String;

@link(symbol := "sys.target.family")
native "musi" let targetFamilyIntrinsic () : String;

@link(symbol := "sys.target.pointer_width")
native "musi" let targetPointerWidthIntrinsic () : Int;

@link(symbol := "sys.target.endian")
native "musi" let targetEndianIntrinsic () : String;

@link(symbol := "sys.jit.supported")
native "musi" let jitSupportedIntrinsic () : Int;

@link(symbol := "sys.jit.backend")
native "musi" let jitBackendIntrinsic () : String;

@link(symbol := "sys.jit.isa")
native "musi" let jitIsaIntrinsic () : String;

@link(symbol := "sys.matches.os")
native "musi" let matchesOsIntrinsic (value : String) : Bool;

@link(symbol := "sys.matches.arch")
native "musi" let matchesArchIntrinsic (value : String) : Bool;

@link(symbol := "sys.matches.family")
native "musi" let matchesFamilyIntrinsic (value : String) : Bool;

--- Raw target operating system name.
export let os : String := unsafe {
  targetOsIntrinsic();
};
--- Raw target architecture name.
export let arch : String := unsafe {
  targetArchIntrinsic();
};
--- Raw target architecture family name.
export let archFamily : String := unsafe {
  targetArchFamilyIntrinsic();
};
--- Raw target platform family name.
export let family : String := unsafe {
  targetFamilyIntrinsic();
};
--- Target pointer width in bits.
export let pointerWidth : Int := unsafe {
  targetPointerWidthIntrinsic();
};
--- Target byte order name.
export let endian : String := unsafe {
  targetEndianIntrinsic();
};

--- Raw JIT support facts.
export let jit :=
  {
    supported := unsafe {
    jitSupportedIntrinsic();
  } > 0,
    backend := unsafe {
    jitBackendIntrinsic();
  },
    isa := unsafe {
    jitIsaIntrinsic();
  },
  };

--- Raw target fact record.
export let target :=
  {
    os := os,
    arch := arch,
    archFamily := archFamily,
    family := family,
    pointerWidth := pointerWidth,
    endian := endian,
    jit := jit,
  };

--- Returns true when value names current OS.
export let matchesOs (value : String) : Bool := unsafe {
  matchesOsIntrinsic(value);
};
--- Returns true when value names current architecture.
export let matchesArch (value : String) : Bool := unsafe {
  matchesArchIntrinsic(value);
};
--- Returns true when value names current platform family.
export let matchesFamily (value : String) : Bool := unsafe {
  matchesFamilyIntrinsic(value);
};

--- Returns true on Unix-family targets.
export let isUnix () : Bool := matchesFamily("unix");
--- Returns true on Darwin-family targets.
export let isDarwin () : Bool := matchesFamily("darwin");
--- Returns true on Windows targets.
export let isWindows () : Bool := matchesFamily("windows");
--- Returns true on WebAssembly-family targets.
export let isWebAssembly () : Bool := matchesFamily("webassembly");
--- Returns true when current target supports JIT.
export let supportsJit () : Bool := jit.supported;
