---
title: "Enums, Variants, and Patterns"
description: "Translate C++17 enum class and std::variant habits into Musi data variants and match."
group: "Musi for Developers"
section: "C++ Developers"
order: 9
slug: "enums-variants-patterns"
summary: "Use data variants when alternatives carry distinct shapes."
---

# Enums, Variants, and Patterns

C++17 can model alternatives with `std::variant` and a visitor:

```cpp
struct Configured {
    int port;
};

struct Default {};

using Port = std::variant<Configured, Default>;

const auto selected = Port{Configured{8080}};
const auto port = std::visit([](const auto& state) -> int {
    using State = std::decay_t<decltype(state)>;
    if constexpr (std::is_same_v<State, Configured>) {
        return state.port;
    }
    return 3000;
}, selected);
```

Musi makes alternatives data variants and matches them directly.

{{snippet:cpp17-enums-variants-patterns}}
