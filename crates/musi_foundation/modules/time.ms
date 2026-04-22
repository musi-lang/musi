let Core := import "musi:core";
let Int := Core.Int;
let Unit := Core.Unit;

export opaque let Time := effect {
  let nowUnixMs () : Int;
  let monotonicMs () : Int;
  let sleepMs (ms : Int) : Unit;
};

export let nowUnixMs () : Int := ask Time.nowUnixMs();
export let monotonicMs () : Int := ask Time.monotonicMs();
export let sleepMs (ms : Int) : Unit := ask Time.sleepMs(ms);
