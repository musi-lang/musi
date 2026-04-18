let core := import "./_core.ms";

--- Span of time in milliseconds.
export let Duration := core.Duration;

--- Wall-clock Unix time in milliseconds.
export let UnixTime := core.UnixTime;

--- Monotonic instant in milliseconds.
export let Instant := core.Instant;

--- Duration from milliseconds.
export let millis := core.millis;

--- Duration from seconds.
export let seconds := core.seconds;

--- Duration from minutes.
export let minutes := core.minutes;

--- Duration from hours.
export let hours := core.hours;

--- Unix wall time now.
export let nowUnix := core.nowUnix;

--- Monotonic instant now.
export let monotonic := core.monotonic;

--- Sleep for a duration.
export let sleep := core.sleep;

--- Duration as milliseconds.
export let(self : Duration).toMillis () : Int := core.durationMillis(self);

--- Duration as whole seconds.
export let(self : Duration).toSeconds () : Int := core.durationSeconds(self);

--- Duration as whole minutes.
export let(self : Duration).toMinutes () : Int := core.durationMinutes(self);

--- Unix wall time as milliseconds.
export let(self : UnixTime).toMillis () : Int := core.unixTimeMillis(self);

--- Unix wall time as whole seconds.
export let(self : UnixTime).toSeconds () : Int := core.unixTimeSeconds(self);

--- Monotonic instant as milliseconds.
export let(self : Instant).toMillis () : Int := core.instantMillis(self);
