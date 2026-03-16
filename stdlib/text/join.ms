//! String join utilities.

import "@std/rt" as rt;

export let join := (arr, sep) => rt.str_join(arr, sep);
