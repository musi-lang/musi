//! Simple leveled logging.

import "@std/rt" as rt;

export let Level := choice { Debug + Info + Warn + Error };

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

export let new := (min_level) => .{ min_level := min_level, prefix := "" };

export let with_prefix := (logger, prefix) => .{ min_level := logger.min_level, prefix := prefix };

export let log := (logger, level, msg) => (
  (
    let tag := level_str(level);
    let pfx := (f"[{logger.prefix}] " if rt.str_len(logger.prefix) > 0 | "" if _);
    rt.writeln(f"{pfx}{tag}: {msg}")
  ) if level_num(level) >= level_num(logger.min_level)
  | () if _
);

export let debug := (logger, msg) => log(logger, .Debug, msg);

export let info := (logger, msg) => log(logger, .Info, msg);

export let warn := (logger, msg) => log(logger, .Warn, msg);

export let error := (logger, msg) => log(logger, .Error, msg);
