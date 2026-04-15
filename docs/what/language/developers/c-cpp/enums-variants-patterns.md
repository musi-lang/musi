---
title: "Enums, Variants, and Patterns"
description: "Translate C enums and C++ variants into Musi data variants and match patterns."
group: "Musi for Developers"
section: "C/C++ Developers"
order: 9
slug: "enums-variants-patterns"
summary: "Model alternatives with payload-carrying variants and inspect them with match."
---

# Enums, Variants, and Patterns

C enums name integer-like states. C++ scoped enums improve names, and `std::variant` can carry payloads. Musi data variants make the alternatives and payloads part of one type.

C needs a tag plus a payload convention:

```c
enum TaskTag {
    TASK_WAITING,
    TASK_RUNNING,
    TASK_DONE,
};

struct TaskState {
    enum TaskTag tag;
    int payload;
};

struct TaskState state = {
    .tag = TASK_RUNNING,
    .payload = 42,
};
```

C++ can put payload alternatives into `std::variant`:

```cpp
struct Waiting {};
struct Running { int id; };
struct Done { int code; };

using TaskState = std::variant<Waiting, Running, Done>;

auto state = TaskState{Running{.id = 42}};
```

Musi variants name each alternative and its payload in one data definition.

{{snippet:c-cpp-enums-variants-patterns}}

Each variant states its payload shape:

- `.Waiting` carries no extra data.
- `.Running(id := 42)` carries the running task id.
- `.Done(code := value)` carries the completion code.

`match` makes each case visible at the use site.
