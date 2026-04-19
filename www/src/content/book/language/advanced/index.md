---
title: "Advanced and Tooling"
description: "Finish with attributes, foreign bindings, quote, testing, and tool workflow."
group: "Advanced and Tooling"
section: "Advanced and Tooling"
order: 8
slug: "advanced"
summary: "Keep sharp or advanced topics late, after ordinary code already feels natural."
---

Advanced and Tooling comes after ordinary Musi code feels readable. These chapters cover sharp edges and project-scale support: attributes, foreign declarations, unsafe and FFI boundaries, operators, quote and syntax tools, compile-time work, templates, testing, and command workflow.

The common theme is boundary control. Attributes add metadata for tools. Foreign declarations describe symbols outside normal Musi source. Unsafe and FFI code crosses places the compiler cannot fully protect. Quote, syntax, templates, and splices let code talk about code. Testing and tooling keep those choices repeatable.

Do not read this part as a bag of clever tricks. Each feature should solve a concrete maintenance problem. A foreign wrapper can isolate a native call. A template can remove regular boilerplate. A test can freeze an important behavior. A Makefile or package script can make the same command work for every contributor.

A good advanced feature leaves the resulting code easier to inspect. If a template makes diagnostics confusing, the template is too clever. If an attribute hides ordinary domain policy, the policy belongs in a value or type. If unsafe code spreads through the package, it needs a smaller wrapper.

By the end, you should know when to stay with ordinary code and when an advanced boundary earns its place.
