---
title: "Null, Option, and Result"
description: "Translate C++17 null pointers, optional values, and fallible returns into Musi Option and Result values."
group: "Musi for Developers"
section: "C++ Developers"
order: 7
slug: "null-option-result"
summary: "Use Option for expected absence and Result for fallible values."
---

# Null, Option, and Result

C++17 uses `std::optional` when absence is expected:

```cpp
auto lookup_port(const std::string_view name) -> std::optional<int> {
    if (name == "admin") {
        return 9000;
    }
    return std::nullopt;
}

const auto port = lookup_port("web").value_or(8080);
```

Musi uses `Option` when absence is part of the value.

{{snippet:cpp17-null-option}}

C++17 projects often define a small result object when exceptions are not the right fit:

```cpp
struct ParseResult {
    bool ok;
    int port;
    std::string error;
};
```

Musi uses `Result` when failure is part of the returned value.

{{snippet:cpp17-result-value}}
