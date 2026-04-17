---
title: "Structs, Classes, and Records"
description: "Translate C++17 structs and simple classes into Musi records and record updates."
group: "Musi for Developers"
section: "C++ Developers"
order: 5
slug: "structs-classes-records"
summary: "C++ structs map to Musi records with named fields and explicit updates."
---

# Structs, Classes, and Records

C++17 records simple data with structs or small classes:

```cpp
struct Endpoint {
    std::string host;
    int port;
    bool secure;
};

const auto local = Endpoint{"localhost", 8080, false};
auto public_endpoint = local;
public_endpoint.host = "api.example.com";
public_endpoint.secure = true;

const auto port = public_endpoint.port;
```

Musi keeps the record shape local and uses record update syntax.

{{snippet:cpp17-structs-classes-records}}
