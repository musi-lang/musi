# How to Musi examples

These examples are ordered as a beginner-to-intermediate playground for people coming from Python, JavaScript, or TypeScript.

For a 25-row concept map with standard-library source references, see [`LEARNING_MAP.md`](./LEARNING_MAP.md). For tooling and agents, use [`learning-map.json`](./learning-map.json).

Each numbered directory is:

- runnable as a package with `cargo run -p musi -- run examples/<name>`
- small enough to edit directly
- focused on one concept or demo shape
- written with Musi top-level execution: the bottom call runs the example

Run every example:

```sh
scripts/run-examples.sh
```

The GLFW example opens a native window and waits until you close it:

```sh
MUSI_RUN_INTERACTIVE_EXAMPLES=1 scripts/run-examples.sh
```

Fetch the OBJ asset for the rasterizer playground:

```sh
examples/24-software-rasterizer/fetch-assets.sh
```

Run one example:

```sh
cargo run -p musi -- run examples/00-hello-musi
```

Native library setup for windowing, graphics, SQLite, and FFI examples:

```sh
scripts/examples-native-deps.sh --print
scripts/examples-native-deps.sh --install
```

## Start here

- `00-hello-musi` — one exported value and one bottom call
- `01-values-and-types` — integers, strings, fixed-width naturals, tuples
- `02-arithmetic-and-booleans` — arithmetic and comparisons
- `03-functions-and-calls` — named functions and nested calls
- `04-if-expressions` — expression-style branching
- `05-sequences-and-tuples` — list-like sequences and tuple values
- `06-blocks-and-bottom-calls` — local lets and final expression blocks

## Build programs

- `07-data-variants-and-match` — data variants and pattern matching
- `08-maybe-style-results` — optional result shape
- `09-closures-and-callback-shapes` — closures and callback-style parameters
- `10-stdlib-math` — `@std/math` helpers
- `11-stdlib-text` — `@std/text` helpers

## Small app models

- `12-number-guess-game` — basic game scoring and hints
- `13-text-adventure-room` — room data for a text adventure
- `14-todo-list-model` — editable app state shape
- `15-mini-database-rows` — typed rows for data-backed apps

## Native, windows, and graphics

- `16-sqlite3-interop` — SQLite foreign declarations
- `17-c-struct-pointer-interop` — C pointer API shape
- `18-process-command` — child process launch from Musi
- `19-glfw-window` — GLFW window lifecycle, `let recur` event loop, and `let ... else` init handling
- `20-sdl2-input-window` — SDL2 input/window declarations
- `21-opengl-triangle` — OpenGL draw declarations and triangle data
- `22-opengl-cube-data` — cube vertex data and buffer declarations
- `23-shader-pipeline` — shader/program declarations
- `24-software-rasterizer` — GLFW/OpenGL monkey OBJ playground with camera controls
