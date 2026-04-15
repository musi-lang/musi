---
title: "Null, Option, and Result"
description: "Translate null pointers, optional values, expected failure, and status codes into Musi Option and Result."
group: "Musi for Developers"
section: "C/C++ Developers"
order: 7
slug: "null-option-result"
summary: "Use Option for absence and Result for failure that carries information."
---

# Null, Option, and Result

C uses null pointers and integer status codes. C++ code may use null pointers, `std::optional`, `std::expected`, exceptions, or out parameters. Musi ordinary code should make absence and failure part of the type.

## Absence is Option

C often uses a null pointer to mean "not found":

```c
const int *lookup_port(const char *name) {
    if (strcmp(name, "admin") == 0) {
        static int port = 9000;
        return &port;
    }
    return NULL;
}
```

C++ can make absence part of the type:

```cpp
auto lookup_port(std::string_view name) -> std::optional<int> {
    if (name == "admin") {
        return 9000;
    }
    return std::nullopt;
}
```

Musi uses the stdlib `Option` shape for ordinary absence.

{{snippet:c-cpp-null-option}}

`Option.Option[Int]` says the port may be present or absent. The caller must choose how to handle that case.

## Failure with information is Result

C commonly splits success from output through a status code and an out pointer:

```c
bool parse_port(const char *text, int *out_port) {
    if (strcmp(text, "8080") == 0) {
        *out_port = 8080;
        return true;
    }
    return false;
}
```

C++23 code can use an expected value:

```cpp
auto parse_port(std::string_view text) -> std::expected<int, std::string> {
    if (text == "8080") {
        return 8080;
    }
    return std::unexpected("invalid port");
}
```

Musi uses `Result` when the failure carries information.

{{snippet:c-cpp-result-value}}

Use `Option` when absence is the whole story. Use `Result` when the caller should receive an error value.
