---
title: "Structs, Records, and Field Updates"
description: "Translate Go structs and struct literals into Musi records and field updates."
group: "Musi for Developers"
section: "Go Developers"
order: 5
slug: "structs-records-field-updates"
summary: "Keep field names visible when constructing and updating data."
---

# Structs, Records, and Field Updates

Go structs make compact named data:

```go
type Endpoint struct {
    Host   string
    Port   int
    Secure bool
}

local := Endpoint{Host: "localhost", Port: 8080, Secure: false}
publicEndpoint := local
publicEndpoint.Host = "api.example.com"
publicEndpoint.Secure = true
```

Musi record-style data keeps field names visible at construction and update sites.

{{snippet:go-structs-records}}

Use record updates when the new value is mostly the old value with a few changed fields.
