let timeHost := import "musi:time";

--- Internal implementation for `millisPerSecond`.
export let millisPerSecond := comptime 1000;
--- Internal implementation for `millisPerMinute`.
export let millisPerMinute := comptime 60 * millisPerSecond;
--- Internal implementation for `millisPerHour`.
export let millisPerHour := comptime 60 * millisPerMinute;

--- Span of time in milliseconds.
export opaque let Duration := data {
  | Duration(millis : Int)
};

--- Wall-clock Unix time in milliseconds.
export opaque let UnixTime := data {
  | UnixTime(millis : Int)
};

--- Monotonic instant in milliseconds.
export opaque let Instant := data {
  | Instant(millis : Int)
};

--- Duration from milliseconds.
export let millis (value : Int) : Duration := .Duration(millis := value);

--- Duration from seconds.
export let seconds (value : Int) : Duration := millis(value * millisPerSecond);

--- Duration from minutes.
export let minutes (value : Int) : Duration := millis(value * millisPerMinute);

--- Duration from hours.
export let hours (value : Int) : Duration := millis(value * millisPerHour);

--- Internal implementation for `durationMillis`.
export let durationMillis (target : Duration) : Int :=
  match target (
  | .Duration(millis := value) => value
  );

--- Internal implementation for `durationSeconds`.
export let durationSeconds (target : Duration) : Int :=
  durationMillis(target) / millisPerSecond;

--- Internal implementation for `durationMinutes`.
export let durationMinutes (target : Duration) : Int :=
  durationMillis(target) / millisPerMinute;

--- Unix wall time now.
export let nowUnix () : UnixTime :=
  .UnixTime(millis := timeHost.nowUnixMs());

--- Internal implementation for `unixTimeMillis`.
export let unixTimeMillis (target : UnixTime) : Int :=
  match target (
  | .UnixTime(millis := value) => value
  );

--- Internal implementation for `unixTimeSeconds`.
export let unixTimeSeconds (target : UnixTime) : Int :=
  unixTimeMillis(target) / millisPerSecond;

--- Monotonic instant now.
export let monotonic () : Instant :=
  .Instant(millis := timeHost.monotonicMs());

--- Internal implementation for `instantMillis`.
export let instantMillis (target : Instant) : Int :=
  match target (
  | .Instant(millis := value) => value
  );

--- Sleep for a duration.
export let sleep (target : Duration) : Unit :=
  timeHost.sleepMs(durationMillis(target));
