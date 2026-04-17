---
title: "Errors, Results, and Effects"
description: "Translate C++17 exceptions, result objects, and host interaction into Musi Result values and effect boundaries."
group: "Musi for Developers"
section: "C++ Developers"
order: 8
slug: "errors-results-effects"
summary: "Keep expected failure as data and outside work behind effectful APIs."
---

# Errors, Results, and Effects

C++17 code often builds a small result object when exceptions are not the right fit:

```cpp
struct ParseResult {
    bool ok;
    int port;
    std::string error;
};

auto parse_port(const std::string_view text) -> ParseResult {
    if (text == "8080") {
        return {true, 8080, ""};
    }
    return {false, 0, "parse error"};
}
```

Musi keeps expected failure in the returned value.

{{snippet:cpp17-errors-results}}

C++ host interaction usually calls library APIs directly:

```cpp
auto name = std::string{};
std::cout << "name> ";
std::getline(std::cin, name);
std::cout << name << '\n';
```

Musi keeps console work at effectful boundaries.

{{snippet:cpp17-effect-boundary}}
