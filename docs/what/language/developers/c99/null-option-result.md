---
title: "Null, Option, and Result"
description: "Translate C99 NULL, sentinels, and status returns into Musi Option and Result values."
group: "Musi for Developers"
section: "C Developers"
order: 7
slug: "null-option-result"
summary: "Use Option for expected absence and Result for fallible values."
---

# Null, Option, and Result

C99 often uses sentinel values for absence:

```c
int lookup_port(const char *name) {
    return name[0] == 'a' ? 9000 : -1;
}

int raw_port = lookup_port("web");
int port = raw_port >= 0 ? raw_port : 8080;
```

Musi uses `Option` when absence is part of the value.

{{snippet:c99-null-option}}

C status returns often pair a code with an out-parameter:

```c
int parse_port(const char *text, int *out_port) {
    if (text[0] == '8') {
        *out_port = 8080;
        return 0;
    }
    return -1;
}
```

Musi uses `Result` when failure is part of the returned value.

{{snippet:c99-result-value}}
