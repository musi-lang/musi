let timeHost := import "musi:time";

--- Core operation for `millisPerSecond`.
export let millisPerSecond := known 1000;
--- Core operation for `millisPerMinute`.
export let millisPerMinute := known 60 * millisPerSecond;
--- Core operation for `millisPerHour`.
export let millisPerHour := known 60 * millisPerMinute;

--- Span of time in milliseconds.
export opaque let Duration := data {
  | DurationMillis(millis : Int)
};

--- Wall-clock Unix time in milliseconds.
export opaque let UnixTime := data {
  | UnixMillis(millis : Int)
};

--- Monotonic instant in milliseconds.
export opaque let Instant := data {
  | InstantMillis(millis : Int)
};

--- Duration from milliseconds.
export let millis (value : Int) : Duration := .DurationMillis(millis := value);

--- Duration from seconds.
export let seconds (value : Int) : Duration := millis(value * millisPerSecond);

--- Duration from minutes.
export let minutes (value : Int) : Duration := millis(value * millisPerMinute);

--- Duration from hours.
export let hours (value : Int) : Duration := millis(value * millisPerHour);

--- Core operation for `durationMillis`.
export let durationMillis (target : Duration) : Int :=
  match target (
  | .DurationMillis(millis := value) => value
  );

--- Core operation for `durationSeconds`.
export let durationSeconds (target : Duration) : Int :=
  durationMillis(target) / millisPerSecond;

--- Core operation for `durationMinutes`.
export let durationMinutes (target : Duration) : Int :=
  durationMillis(target) / millisPerMinute;

--- Unix wall time now.
export let nowUnix () : UnixTime := .UnixMillis(millis := timeHost.nowUnixMs());

--- Core operation for `unixTimeMillis`.
export let unixTimeMillis (target : UnixTime) : Int :=
  match target (
  | .UnixMillis(millis := value) => value
  );

--- Core operation for `unixTimeSeconds`.
export let unixTimeSeconds (target : UnixTime) : Int :=
  unixTimeMillis(target) / millisPerSecond;

--- Monotonic instant now.
export let monotonic () : Instant :=
  .InstantMillis(millis := timeHost.monotonicMs());

--- Core operation for `instantMillis`.
export let instantMillis (target : Instant) : Int :=
  match target (
  | .InstantMillis(millis := value) => value
  );

--- Sleep for a duration.
export let sleep (target : Duration) : Unit :=
  timeHost.sleepMs(durationMillis(target));

--- Duration as milliseconds.
export let (self : Duration).toMillis () : Int := durationMillis(self);

--- Duration as whole seconds.
export let (self : Duration).toSeconds () : Int := durationSeconds(self);

--- Duration as whole minutes.
export let (self : Duration).toMinutes () : Int := durationMinutes(self);

--- Unix wall time as milliseconds.
export let (self : UnixTime).toMillis () : Int := unixTimeMillis(self);

--- Unix wall time as whole seconds.
export let (self : UnixTime).toSeconds () : Int := unixTimeSeconds(self);

--- Monotonic instant as milliseconds.
export let (self : Instant).toMillis () : Int := instantMillis(self);
