---
title: "Structs, Classes, and Records"
description: "Translate structs and classes into Musi records, variants, methods, and behavior contracts."
group: "Musi for Developers"
section: "C/C++ Developers"
order: 5
slug: "structs-classes-records"
summary: "Separate data shape from behavior so construction and updates stay visible."
---

# Structs, Classes, and Records

C structs group fields. C++ classes add constructors, methods, visibility, inheritance, and destructors. Musi separates the data shape from behavior:

- record-style data holds named fields;
- variant-style data models alternatives;
- methods and functions attach behavior where it helps reading;
- classes and instances express shared behavior contracts.

C struct setup names each field at the call site:

```c
struct Endpoint {
    const char *host;
    int port;
    bool secure;
};

struct Endpoint local = {
    .host = "localhost",
    .port = 8080,
    .secure = false,
};
```

C++ can model the same data with aggregate initialization:

```cpp
struct Endpoint {
    std::string host;
    int port;
    bool secure;
};

auto local = Endpoint{
    .host = "localhost",
    .port = 8080,
    .secure = false,
};
```

Musi keeps record construction and record update visible.

{{snippet:c-cpp-structs-classes-records}}

The record literal says exactly which fields are present. The update form copies the existing record and replaces selected fields.

## Constructors are ordinary values

C++ constructors often hide field setup inside overloads. Musi prefers visible construction. If construction needs validation, write a function that returns `Option` or `Result`.
