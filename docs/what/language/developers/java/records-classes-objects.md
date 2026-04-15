---
title: "Records, Classes, and Objects"
description: "Translate Java records and classes into Musi records, variants, methods, and behavior contracts."
group: "Musi for Developers"
section: "Java Developers"
order: 5
slug: "records-classes-objects"
summary: "Separate data shape from behavior so construction and updates stay visible."
---

# Records, Classes, and Objects

Java 17 records make small immutable data carriers concise:

```java
record Endpoint(String host, int port, boolean secure) {}

var local = new Endpoint("localhost", 8080, false);
var publicEndpoint = new Endpoint("api.example.com", local.port(), true);
```

Musi record-style data keeps the field names visible at construction and update sites.

{{snippet:java-records-classes-objects}}

Java classes combine state and behavior. Musi separates the data shape from functions, methods, classes, and instances so each part stays visible.

