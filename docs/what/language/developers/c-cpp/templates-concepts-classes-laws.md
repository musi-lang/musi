---
title: "Templates, Concepts, Classes, and Laws"
description: "Translate templates and concepts into Musi type parameters, classes, instances, and laws."
group: "Musi for Developers"
section: "C/C++ Developers"
order: 10
slug: "templates-concepts-classes-laws"
summary: "Use generic functions for reuse and classes with laws for behavior contracts."
---

# Templates, Concepts, Classes, and Laws

C uses macros and void pointers for some generic designs. C++ templates and concepts express reusable code and constraints. Musi uses type parameters for generic functions and classes for shared behavior.

## Generic functions

C sometimes uses macros to avoid repeating a tiny operation per type:

```c
#define IDENTITY(value) (value)

int port = IDENTITY(8080);
```

C++ templates give the operation a typed generic form:

```cpp
template <class T>
auto identity(T input) -> T {
    return input;
}

auto port = identity<int>(8080);
```

Musi uses explicit type parameters for the same reusable shape.

{{snippet:c-cpp-generic-function}}

The type parameter is written at the function and at the call. In larger code, inference can keep many call sites lighter.

## Classes and laws

C++ concepts can say that a type supports certain operations. Musi classes do the same kind of work, and laws record behavior promises that instances should satisfy.

C has no native concept system, so projects usually document the required functions by convention:

```c
struct Car {};

int car_wheels(struct Car car) {
    return 4;
}
```

C++ concepts can state the required operation:

```cpp
template <class T>
concept Vehicle = requires(T vehicle) {
    { wheels(vehicle) } -> std::same_as<int>;
};
```

Musi classes state the operation and can record a law for the behavior expectation.

{{snippet:c-cpp-concept-class-law}}

A real-world analogy: a car is a vehicle, but by road law it must have at least four wheels. The class names the operation. The law records the expectation that makes implementations trustworthy.
