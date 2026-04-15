---
title: "Types, Generics, and Protocols"
description: "Translate Python annotations, generics, and Protocol habits into Musi type parameters, classes, and instances."
group: "Musi for Developers"
section: "Python Developers"
order: 10
slug: "types-generics-protocols"
summary: "Use generics for reusable value shape and classes for shared behavior."
---

Python typing can describe a reusable function:

```python
def identity[T](input: T) -> T:
    return input

port = identity[int](8080)
port
```

Musi type parameters sit beside the function name.

{{snippet:python-generic-function}}

The type parameter says the function works the same way for any `T`.

## Protocol habits

Python `Protocol` names behavior that many types can satisfy:

```python
from typing import Protocol

class Sized(Protocol):
    def size(self) -> int: ...
```

Musi uses a `class` for the behavior shape and an `instance` for a type that provides it.

{{snippet:python-protocol-class}}

Use a class when callers need behavior, not a particular concrete data type.
