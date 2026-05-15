# How to Musi learning map

This file is a machine-readable-by-humans guide for agents and readers. Each example teaches one distinct Musi concept and points at the standard-library source that shows the same idea in production code.

| #   | Example                           | Primary lesson                                                            | Standard-library references                                                               |
| --- | --------------------------------- | ------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------- |
| 00  | `00-hello-musi`                   | Top-to-bottom execution, imports, `io.writeLn` output                     | `lib/std/io.ms`                                                                           |
| 01  | `01-values-and-types`             | Value bindings, type annotations, scalar literals, tuples                 | `lib/std/prelude.ms`, `lib/std/std.ms`                                                    |
| 02  | `02-arithmetic-and-booleans`      | Arithmetic, comparison expressions, `Bit` results                         | `lib/std/cmp.ms`, `lib/std/math.ms`                                                       |
| 03  | `03-functions-and-calls`          | Function declarations, parameters, nested calls                           | `lib/std/math.ms`, `lib/std/fmt.ms`                                                       |
| 04  | `04-if-expressions`               | `if ... then ... else ...` as an expression                               | `lib/std/cmp.ms`, `lib/std/ascii.ms`                                                      |
| 05  | `05-sequences-and-tuples`         | Array-like sequence literals, nested sequences, tuples                    | `lib/std/collections.ms`, `lib/std/collections/array.ms`                                  |
| 06  | `06-blocks-and-bottom-calls`      | Sequence blocks, local `let`, final expression result                     | `lib/std/process.ms`, `lib/std/path.ms`                                                   |
| 07  | `07-data-variants-and-match`      | `data` variants and `match` destructuring                                 | `lib/std/cmp.ms`, `lib/std/path.ms`                                                       |
| 08  | `08-maybe-style-results`          | Maybe-style sum types and fallback handling                               | `lib/std/maybe.ms`                                                                        |
| 09  | `09-closures-and-callback-shapes` | Closure capture and callable values                                       | `lib/std/text.ms`, `lib/std/encoding/hex.ms`                                              |
| 10  | `10-stdlib-math`                  | Reusing `@std/math` helpers                                               | `lib/std/math.ms`                                                                         |
| 11  | `11-stdlib-text`                  | Reusing `@std/text` helpers and string methods                            | `lib/std/text.ms`, `lib/std/ascii.ms`                                                     |
| 12  | `12-number-guess-game`            | Small game logic with variants, stdlib math, and hints                    | `lib/std/math.ms`, `lib/std/fmt.ms`                                                       |
| 13  | `13-text-adventure-room`          | Modeling app/world state with data variants                               | `lib/std/path.ms`, `lib/std/json.ms`                                                      |
| 14  | `14-todo-list-model`              | Immutable state transformation and boolean fields                         | `lib/std/collections/list.ms`, `lib/std/testing.ms`                                       |
| 15  | `15-mini-database-rows`           | Typed record-like row modeling for persistence                            | `lib/std/json.ms`, `lib/std/fs.ms`                                                        |
| 16  | `16-sqlite3-interop`              | C ABI declarations, `CString`, `CPtr`, stdlib null pointer helpers        | `lib/std/ffi.ms`, `lib/std/libc.ms`                                                       |
| 17  | `17-c-struct-pointer-interop`     | Opaque C pointer API shape and pointer ownership boundaries               | `lib/std/ffi.ms`                                                                          |
| 18  | `18-process-command`              | Running a child process from Musi                                         | `lib/std/process.ms`, `lib/std/cli.ms`                                                    |
| 19  | `19-glfw-window`                  | Direct GLFW `@foreign` bindings, `let recur` event loop, `defer` cleanup  | `lib/std/ffi.ms`, `lib/std/io.ms`                                                         |
| 20  | `20-sdl2-input-window`            | SDL2 window/input ABI surface                                             | `lib/std/ffi.ms`, `lib/std/os.ms`                                                         |
| 21  | `21-opengl-triangle`              | OpenGL draw-call declarations and vertex data                             | `lib/std/ffi.ms`, `lib/std/math.ms`                                                       |
| 22  | `22-opengl-cube-data`             | Mesh buffers, cube vertices, OpenGL buffer declarations                   | `lib/std/ffi.ms`, `lib/std/bytes.ms`                                                      |
| 23  | `23-shader-pipeline`              | Shader/program pipeline declarations and C string source                  | `lib/std/ffi.ms`, `lib/std/text.ms`                                                       |
| 24  | `24-software-rasterizer`          | GLFW/OpenGL viewer loop, OBJ asset loading, camera state, `defer` cleanup | `lib/std/ffi.ms`, `lib/std/fs.ms`, `lib/std/path.ms`, `lib/std/text.ms`, `lib/std/fmt.ms` |

## Agent checklist

- Read examples in numeric order.
- Run one example with `cargo run -p musi -- run examples/<directory>`.
- Use `scripts/run-examples.sh` for non-interactive validation.
- Use `MUSI_RUN_INTERACTIVE_EXAMPLES=1 scripts/run-examples.sh` for windowed examples.
- Use `examples/24-software-rasterizer/fetch-assets.sh` before the OBJ playground.
- Compare each concept with the referenced `lib/std` file before changing syntax or style.
