--! Date and time module.

let timeHost := import "musi:time";

--- Milliseconds per second.
export let millisPerSecond := known 1000;
--- Milliseconds per minute.
export let millisPerMinute := known 60 * millisPerSecond;
--- Milliseconds per hour.
export let millisPerHour := known 60 * millisPerMinute;

--- Span of time in milliseconds.
export hidden let Duration := data {
  | DurationMillis(millis : Int)
};

--- Wall-clock Unix time in milliseconds.
export hidden let UnixTime := data {
  | UnixMillis(millis : Int)
};

--- Monotonic instant in milliseconds.
export hidden let Instant := data {
  | InstantMillis(millis : Int)
};

--- Duration from milliseconds.
export let millis (value : Int) := .DurationMillis(millis := value);

--- Duration from seconds.
export let seconds (value : Int) := millis(value * millisPerSecond);

--- Duration from minutes.
export let minutes (value : Int) := millis(value * millisPerMinute);

--- Duration from hours.
export let hours (value : Int) := millis(value * millisPerHour);

--- Return duration as milliseconds.
export let durationMillis (target : Duration) :=
  match target (
  | .DurationMillis(millis := value) => value
  );

--- Return duration as whole seconds.
export let durationSeconds (target : Duration) :=
  durationMillis(target) / millisPerSecond;

--- Return duration as whole minutes.
export let durationMinutes (target : Duration) :=
  durationMillis(target) / millisPerMinute;

--- Unix wall time now.
export let nowUnix () := .UnixMillis(millis := timeHost.nowUnixMs());

--- Return Unix wall time as milliseconds.
export let unixTimeMillis (target : UnixTime) :=
  match target (
  | .UnixMillis(millis := value) => value
  );

--- Return Unix wall time as whole seconds.
export let unixTimeSeconds (target : UnixTime) :=
  unixTimeMillis(target) / millisPerSecond;

--- Monotonic instant now.
export let monotonic () := .InstantMillis(millis := timeHost.monotonicMs());

--- Return monotonic instant as milliseconds.
export let instantMillis (target : Instant) :=
  match target (
  | .InstantMillis(millis := value) => value
  );

--- Sleep for a duration.
export let sleep (target : Duration) :=
  timeHost.sleepMs(durationMillis(target));

--- Duration as milliseconds.
export let (self : Duration).toMillis () := durationMillis(self);

--- Duration as whole seconds.
export let (self : Duration).toSeconds () := durationSeconds(self);

--- Duration as whole minutes.
export let (self : Duration).toMinutes () := durationMinutes(self);

--- Unix wall time as milliseconds.
export let (self : UnixTime).toMillis () := unixTimeMillis(self);

--- Unix wall time as whole seconds.
export let (self : UnixTime).toSeconds () := unixTimeSeconds(self);

--- Monotonic instant as milliseconds.
export let (self : Instant).toMillis () := instantMillis(self);
