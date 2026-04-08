# Effect System

Normative effect-system reference for the current language surface.

## Core Model

Musi uses resumable algebraic effects as a first-class language feature.

The core surface is:

- `effect`
- `perform`
- `handle`
- `with`
- `resume`

Effects are not an exception-only subsystem and they are not library sugar over ordinary functions.

Effects are a computation feature. The grammar no longer reserves a separate type-only surface; semantic checking decides where effectful expressions are valid.

## Effect Declarations

Effects are declared as expressions bound through `let`.

```musi
let Console := effect {
  let writeln (msg : String) : Unit;
  let readln () : String;
};

let Abort := effect {
  let abort (msg : String) : Empty;
};

let State[S] := effect {
  let get () : S;
  let put (s : S) : Unit;
};
```

Effect members describe operations. Laws may also be attached as named propositions.

## Performing Effects

`perform` invokes an effect operation and now accepts an ordinary expression operand.

```musi
perform Console.writeln("hello");
let line := perform Console.readln();
perform Abort.abort("bad state");
perform op(msg);
```

The operation signature determines the value expected back from the handler continuation.

## Handling Effects

Handlers use dedicated clause syntax.

```musi
handle work() with Console of (
| value => value
| writeln(msg, k) => (
    foreign_print(msg);
    resume ()
  )
| readln(k) => resume foreign_readline()
)
```

The clauses are:

- one value clause: `value => ...`
- one clause per handled operation: `op(args, k) => ...`

There is no `return x => ...` handler syntax in the current language.

## `resume`

`resume` continues the suspended computation captured by an operation clause.

That makes handlers resumable, not merely exception-like.

Typical meanings:

- zero resumes: abort or replace the computation
- one resume: ordinary continuation
- multiple resumes: backtracking, nondeterminism, or replay-style behavior

`resume` is part of the effect language because Musi keeps resumable handlers as a first-class control model.

## Open Effect Rows

Effect rows are open immediately, not deferred to a later design phase.

Rows live on signatures through `with { ... }`.

```musi
let f (x : Int) with { Console } : Int := ...;
let g[T] (x : T) with { State[T], ...r } : T := ...;
```

### Reading Rows

| Form                           | Meaning                                                |
| ------------------------------ | ------------------------------------------------------ |
| `with { Console }`             | exactly the named visible effect row in this signature |
| `with { State[Int], Console }` | multiple named effects                                 |
| `with { State[Int], ...r }`    | `State[Int]` plus a named remainder row                |

Named remainder syntax uses `...r`.

This is row openness, not “open class”/“sealed class” terminology from OOP languages.

## Pure And Effectful Functions

The language distinguishes:

- pure arrows: `->`
- effectful arrows: `~>`

Signatures then refine effectful computations with `with { ... }`.

The current documentation model is:

- purity is part of function-kind syntax
- effect rows are part of signature syntax

This keeps type-valued forms inside the same grammar as every other expression.

## Effect Handling And Rows

Handling removes named effects from the active row of the handled computation.

Conceptually:

- a computation may perform `E`
- `handle ... with E of (...)` interprets `E`
- outside the handler, `E` no longer appears in the active row unless reintroduced

Open rows matter because generic code can say:

- “I require `State[T]`”
- “and I preserve the caller’s remaining effects as `...r`”

That is the main reason Musi documents open effects immediately instead of using closed sets only.

## Laws On Effects

Effects may also carry `law` declarations when the effect interface has meaningful algebraic obligations.

Those laws are:

- named propositions over operations
- consumed by docs, tooling, and generated property checks

They are not runtime enforcement and they do not change handler dispatch.

## Control Model

The effect system sits inside the current control model:

- control flow is expression-driven
- `if` is guard-only
- `case` remains the eliminator for branching
- non-local control belongs to resumable handlers, not statement-like escape constructs
