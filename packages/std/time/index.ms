let runtime := import "musi:runtime";

export opaque let Duration := data {
  | Millis(value : Int)
};

export let millis (value : Int) : Duration := .Millis(value := value);

export let seconds (value : Int) : Duration := millis(value * 1000);

export let toMillis (duration : Duration) : Int :=
  match duration (
  | .Millis(value := value) => value
  );

export let toSeconds (duration : Duration) : Int := toMillis(duration) / 1000;

export let nowUnixMillis () : Int := runtime.timeNowUnixMs();

export let nowUnixSeconds () : Int := nowUnixMillis() / 1000;

export let monotonicMillis () : Int := runtime.timeMonotonicMs();

export let sleep (duration : Duration) : Unit := runtime.timeSleepMs(toMillis(duration));

export let sleepMillis (ms : Int) : Unit := sleep(millis(ms));
