//! String join utilities.

import "@std/rt" as rt;

/// Join an array of strings into one, inserting sep between each element.
export let join := (arr, sep) => rt.str_join(arr, sep);
