let runtime := import "@std/runtime";

export let stamp () : Int := request runtime.timeNowUnixMs();
