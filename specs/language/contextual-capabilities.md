# Shapes And Capability Values

Status: frozen 0.1.0 host-language baseline

`shape` defines a static interface or contract shape. Musi small core uses ordinary values and parameters for capability flow.

```musi
let Reader := shape {
  let read(into : mut Buffer) : IOError!Nat;
};
```

A concrete value can satisfy a shape statically. An `erased Shape` value may carry runtime evidence, witnesses, or dynamic dispatch. This keeps type-erasure cost visible.

Capability values are ordinary values passed through normal bindings, imports, and parameters. They carry authority explicitly; there is no hidden global instance registry or ambient authority context.

```musi
let useReader(reader : erased Reader, buffer : mut Buffer) : IOError!Nat := (
  reader.read(buffer)
);
```

Operator members may appear in shapes, but operator precedence and associativity are fixed by the grammar, not by user declarations.
