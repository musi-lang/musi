let runtime := import "musi:runtime";

let millisPerSecond := comptime 1000;
let millisPerMinute := comptime 60 * millisPerSecond;
let millisPerHour := comptime 60 * millisPerMinute;

/// Span of time in milliseconds.
export opaque let Duration := data {
  | Duration(millis : Int)
};

/// Wall-clock Unix time in milliseconds.
export opaque let UnixTime := data {
  | UnixTime(millis : Int)
};

/// Monotonic instant in milliseconds.
export opaque let Instant := data {
  | Instant(millis : Int)
};

/// Duration from milliseconds.
export let millis (value : Int) : Duration := .Duration(millis := value);

/// Duration from seconds.
export let seconds (value : Int) : Duration := millis(value * millisPerSecond);

/// Duration from minutes.
export let minutes (value : Int) : Duration := millis(value * millisPerMinute);

/// Duration from hours.
export let hours (value : Int) : Duration := millis(value * millisPerHour);

/// Duration as milliseconds.
export let durationMillis (duration : Duration) : Int :=
  match duration (
  | .Duration(millis := value) => value
  );

/// Public toMillis API.
export let (self : Duration).toMillis () : Int := durationMillis(self);

/// Duration as whole seconds.
export let durationSeconds (duration : Duration) : Int := durationMillis(duration) / millisPerSecond;

/// Duration as whole seconds.
export let (self : Duration).toSeconds () : Int := durationSeconds(self);

/// Duration as whole minutes.
export let durationMinutes (duration : Duration) : Int := durationMillis(duration) / millisPerMinute;

/// Duration as whole minutes.
export let (self : Duration).toMinutes () : Int := durationMinutes(self);

/// Unix wall time now.
export let nowUnix () : UnixTime := .UnixTime(millis := runtime.timeNowUnixMs());

/// Unix wall time as milliseconds.
export let unixTimeMillis (time : UnixTime) : Int :=
  match time (
  | .UnixTime(millis := value) => value
  );

/// Public toMillis API.
export let (self : UnixTime).toMillis () : Int := unixTimeMillis(self);

/// Unix wall time as whole seconds.
export let unixTimeSeconds (time : UnixTime) : Int := unixTimeMillis(time) / millisPerSecond;

/// Unix wall time as whole seconds.
export let (self : UnixTime).toSeconds () : Int := unixTimeSeconds(self);

/// Monotonic instant now.
export let monotonic () : Instant := .Instant(millis := runtime.timeMonotonicMs());

/// Monotonic instant as milliseconds.
export let instantMillis (instant : Instant) : Int :=
  match instant (
  | .Instant(millis := value) => value
  );

/// Public toMillis API.
export let (self : Instant).toMillis () : Int := instantMillis(self);

/// Sleep for a duration.
export let sleep (duration : Duration) : Unit := runtime.timeSleepMs(durationMillis(duration));
