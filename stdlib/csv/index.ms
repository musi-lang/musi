//! CSV parsing and serialization.

/// Parses CSV text into a list of rows, each row a list of field strings.
export let parse := import "./parse";

/// Serializes a list of rows (each a list of strings) into CSV text.
export let stringify := import "./stringify";
