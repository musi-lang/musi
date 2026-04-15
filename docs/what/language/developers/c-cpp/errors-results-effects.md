---
title: "Errors, Results, and Effects"
description: "Translate C status codes and C++ exceptions into Musi Result values and effect boundaries."
group: "Musi for Developers"
section: "C/C++ Developers"
order: 8
slug: "errors-results-effects"
summary: "Keep expected failure as data and outside work behind effectful APIs."
---

# Errors, Results, and Effects

C often returns error codes. C++ often throws exceptions for exceptional paths. Musi keeps expected failure as data and outside work as effects.

C status-code style separates the value from success:

```c
bool parse_port(const char *text, int *out_port) {
    if (strcmp(text, "8080") == 0) {
        *out_port = 8080;
        return true;
    }
    return false;
}
```

C++ exception style hides the error path in control flow:

```cpp
auto parse_port(std::string_view text) -> int {
    if (text == "8080") {
        return 8080;
    }
    throw std::invalid_argument("parse error");
}
```

Musi keeps expected failure in the returned value.

{{snippet:c-cpp-errors-results}}

The parse function returns either a parsed port or an error message. The caller decides whether to unwrap with a fallback, match both cases, or pass the `Result` upward.

## Outside work uses effect boundaries

Console, file, clock, and host interaction are not plain calculations. Musi stdlib helpers expose those operations explicitly.

C boundary code often calls host APIs directly:

```c
char name[128];
fgets(name, sizeof name, stdin);
puts(name);
```

C++ boundary code may wrap the same host interaction in streams:

```cpp
std::string name;
std::getline(std::cin, name);
std::cout << name << '\n';
```

Musi keeps this kind of outside work at effectful boundaries.

{{snippet:c-cpp-effect-boundary}}

Keep pure transformation code separate from effectful boundary code. That makes tests smaller and makes package behavior easier to inspect.
