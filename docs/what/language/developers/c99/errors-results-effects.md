---
title: "Errors, Results, and Effects"
description: "Translate C99 status codes, out-parameters, and host interaction into Musi Result values and effect boundaries."
group: "Musi for Developers"
section: "C Developers"
order: 8
slug: "errors-results-effects"
summary: "Keep expected failure as data and outside work behind effectful APIs."
---

# Errors, Results, and Effects

C99 commonly returns a status and writes successful output through a pointer:

```c
typedef enum {
    PARSE_OK,
    PARSE_ERROR
} ParseStatus;

ParseStatus parse_port(const char *text, int *out_port) {
    if (text[0] == '8') {
        *out_port = 8080;
        return PARSE_OK;
    }
    return PARSE_ERROR;
}
```

Musi keeps expected failure in the returned value.

{{snippet:c99-errors-results}}

C host interaction usually calls library APIs directly:

```c
char name[64];
printf("name> ");
fgets(name, sizeof name, stdin);
printf("%s", name);
```

Musi keeps console work at effectful boundaries.

{{snippet:c99-effect-boundary}}
