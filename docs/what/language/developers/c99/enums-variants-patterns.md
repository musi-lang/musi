---
title: "Enums, Variants, and Patterns"
description: "Translate C99 enum tags and tagged-union habits into Musi data variants and match."
group: "Musi for Developers"
section: "C Developers"
order: 9
slug: "enums-variants-patterns"
summary: "Use data variants when alternatives carry distinct shapes."
---

# Enums, Variants, and Patterns

C99 enum constants are named integers:

```c
typedef enum {
    TASK_WAITING,
    TASK_RUNNING,
    TASK_DONE
} TaskState;

TaskState state = TASK_RUNNING;
int active = state == TASK_RUNNING;
```

Musi makes alternatives data variants and matches them directly.

{{snippet:c99-enums-variants-patterns}}
