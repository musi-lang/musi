// Word frequency counter using Map['K,'V].
// Demonstrates maps, string utilities, and for-loops.

import { map_get_or } from "std/collections/map";
import { string_words } from "std/string";

const text := "the quick brown fox jumps over the lazy dog the fox";

const counts := map_new();
for word in string_words(text) loop (
    const n := unwrap_or(map_get(counts, word), 0);
    map_set(counts, word, n + 1)
);

// Print each word and its count.
for word in map_keys(counts) loop (
    const n := unwrap_or(map_get(counts, word), 0);
    writeln(string_concat(word, string_concat(": ", int_to_string(n))))
);
