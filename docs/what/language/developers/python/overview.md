---
title: "Overview"
description: "Translate Python 3.14 habits into Musi code with side-by-side examples."
group: "Musi for Developers"
section: "Python Developers"
order: 1
slug: "overview"
summary: "Start from Python habits, then read the equivalent Musi expression, data, effect, and package shapes."
---

Python 3.14 is the comparison point for this guide. Each page starts with Python code, then shows Musi code with the same names and the same job.

Python readers often rely on indentation, dynamic objects, `None`, exceptions, and flexible imports. Musi keeps the same practical goals, but makes value flow, data shape, and outside work visible in the code.

## First translation

Python often starts with a small function and a result binding:

```python
def total(base: int, fee: int) -> int:
    return base + fee

answer = total(1200, 45)
answer
```

Musi writes the same calculation as a `let` function. The final expression leaves the value.

{{snippet:python-values-functions}}

Read it like a kitchen ticket: inputs enter the named station, the expression at the end is the dish that leaves.

## Reading path

- [Values, Functions, and Final Expressions](/learn/book/developers/guides/python/values-functions)
- [Blocks, Control Flow, and Repetition](/learn/book/developers/guides/python/blocks-control-flow)
- [Names, Mutation, and Fresh Values](/learn/book/developers/guides/python/names-mutation)
- [Records, Objects, and Dictionaries](/learn/book/developers/guides/python/records-objects-dicts)
- [Collections and Pipelines](/learn/book/developers/guides/python/collections-pipelines)
- [`None`, Option, and Result](/learn/book/developers/guides/python/none-option-result)
- [Exceptions, Results, and Effects](/learn/book/developers/guides/python/exceptions-effects)
- [Data Variants and Pattern Matching](/learn/book/developers/guides/python/data-variants-patterns)
- [Types, Generics, and Protocols](/learn/book/developers/guides/python/types-generics-protocols)
- [Modules and Packages](/learn/book/developers/guides/python/modules-packages)
- [Testing and Tooling](/learn/book/developers/guides/python/testing-tooling)
- [Native Boundaries, Unsafe, and FFI](/learn/book/developers/guides/python/native-unsafe-ffi)

## Python habits that transfer

- name functions after domain work
- keep data close to behavior that reads it
- use tests as executable examples
- make package boundaries clear
- prefer readable code over clever compression

## Python habits to translate

- `return` becomes the final expression
- indentation blocks become expression blocks with braces or parentheses
- `None` becomes `Option`
- exceptions become `Result` when failure is ordinary data
- async and outside work become effect requests or stdlib runtime helpers
- duck typing and `Protocol` become classes, instances, and laws when behavior needs rules
