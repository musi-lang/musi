---
title: "Macros, Generics, Classes, and Laws"
description: "Translate C99 macro-style generic helpers and behavior conventions into Musi type parameters and classes."
group: "Musi for Developers"
section: "C Developers"
order: 10
slug: "templates-concepts-classes-laws"
summary: "Use type parameters for reusable values and classes for reusable behavior."
---

# Macros, Generics, Classes, and Laws

C99 reaches for macros when one operation should work across repeated shapes:

```c
#define IDENTITY_INT(input) (input)

int answer = IDENTITY_INT(21);
```

Musi uses type parameters instead of macro expansion for reusable values.

{{snippet:c99-generic-function}}

C libraries often describe required behavior by convention and documentation:

```c
typedef struct Vehicle Vehicle;

typedef struct {
    int (*wheels)(const Vehicle *self);
} VehicleOps;
```

Musi classes state the required operations and laws directly.

{{snippet:c99-concept-class-law}}
