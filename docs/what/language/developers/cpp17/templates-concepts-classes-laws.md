---
title: "Templates, Concepts, Classes, and Laws"
description: "Translate C++17 templates and behavior constraints into Musi type parameters, classes, and laws."
group: "Musi for Developers"
section: "C++ Developers"
order: 10
slug: "templates-concepts-classes-laws"
summary: "Use type parameters for reusable values and classes for reusable behavior."
---

# Templates, Concepts, Classes, and Laws

C++17 templates express reusable value-level operations:

```cpp
template <class T>
auto identity(T input) -> T {
    return input;
}

const auto answer = identity(21);
```

Musi uses type parameters for reusable values.

{{snippet:cpp17-generic-function}}

C++ expresses required behavior through templates, traits, and concepts in later standards:

```cpp
template <class T>
auto wheels(const T& vehicle) -> int {
    return vehicle.wheels();
}
```

Musi classes state the required operations and laws directly.

{{snippet:cpp17-concept-class-law}}
