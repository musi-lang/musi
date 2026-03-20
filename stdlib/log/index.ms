//! Simple leveled logging.

import "@std/rt" as rt;

/// Log severity level.
export let Level := choice { Debug + Info + Warn + Error };

/// Logger configuration holding the minimum level and an optional prefix.
export let Logger := record { min_level : Level; prefix : String };

let level_num := (lvl) => match lvl (
  .Debug => 0
  | .Info  => 1
  | .Warn  => 2
  | .Error => 3
);

let level_str := (lvl) => match lvl (
  .Debug => "DEBUG"
  | .Info  => "INFO"
  | .Warn  => "WARN"
  | .Error => "ERROR"
);

/// Creates a logger that only emits messages at or above min_level.
export let new := (min_level) => .{ min_level := min_level, prefix := "" };

/// Returns a copy of logger with the given prefix string.
export let with_prefix := (logger, prefix) => .{ min_level := logger.min_level, prefix := prefix };

/// Emits a message at the given level if it meets the logger's minimum.
export let log := (logger, level, msg) => (
  (
    let tag := level_str(level);
    let pfx := (f"[{logger.prefix}] " if rt.str_len(logger.prefix) > 0 | "" if _);
    rt.writeln(f"{pfx}{tag}: {msg}")
  ) if level_num(level) >= level_num(logger.min_level)
  | () if _
);

/// Emits a DEBUG message.
export let debug := (logger, msg) => log(logger, .Debug, msg);

/// Emits an INFO message.
export let info := (logger, msg) => log(logger, .Info, msg);

/// Emits a WARN message.
export let warn := (logger, msg) => log(logger, .Warn, msg);

/// Emits an ERROR message.
export let error := (logger, msg) => log(logger, .Error, msg);
