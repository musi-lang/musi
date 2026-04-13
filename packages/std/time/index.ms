let Runtime := import "musi:runtime";

export let nowUnixMillis () : Int := Runtime.timeNowUnixMs();

export let nowUnixSeconds () : Int := nowUnixMillis() / 1000;

export let monotonicMillis () : Int := Runtime.timeMonotonicMs();

export let sleepMillis (ms : Int) : Unit := Runtime.timeSleepMs(ms);
