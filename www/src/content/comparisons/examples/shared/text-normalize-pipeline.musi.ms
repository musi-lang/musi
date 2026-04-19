let text := import "@std/text";

let normalized := "  Ada  " |> text.trim() |> text.toLowerCase();
normalized;
