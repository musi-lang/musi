foreign "c" (
  let clock_ms () : Int;
);

export let nativeClockMs () : Int := unsafe {
  clock_ms();
};
