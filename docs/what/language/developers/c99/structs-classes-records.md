---
title: "Structs, Classes, and Records"
description: "Translate C99 structs and field updates into Musi records and record updates."
group: "Musi for Developers"
section: "C Developers"
order: 5
slug: "structs-classes-records"
summary: "C structs map to Musi records with named fields and explicit updates."
---

# Structs and Records

C99 records named fields with a `struct`, then updates fields directly:

```c
typedef struct {
    const char *host;
    int port;
    int secure;
} Endpoint;

Endpoint local = {"localhost", 8080, 0};
Endpoint public_endpoint = local;
public_endpoint.host = "api.example.com";
public_endpoint.secure = 1;

int port = public_endpoint.port;
```

Musi keeps the record shape local and uses record update syntax.

{{snippet:c99-structs-classes-records}}
