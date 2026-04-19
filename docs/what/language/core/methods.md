---
title: "Dot Calls"
description: "Learn Musi dot-callable model after plain functions and calls."
group: "Core Syntax"
section: "Core Syntax"
order: 11
slug: "dot-calls"
summary: "Use dot-callable functions through dot notation without needing an impl block."
---
Methods let a value carry useful actions near its type. A `Person` can expose a display name, a `Cart` can compute a total, and a `Dog` can report whether it needs a walk.

{{snippet:chapter-methods}}

Method syntax reads from the value outward. That makes chains natural when each step transforms the same idea, such as trimming a name and turning it into a normalized key.

## Good receiver choices

Put a method on the type that owns the idea. A car should know whether its fuel is low; a payment processor should not be hidden inside a car. When the receiver is obvious in ordinary speech, the method usually belongs there.

## Avoiding mystery chains

Chains become hard when each method changes the subject. Keep method chains for steady transformations. If a chain jumps from customer to order to warehouse to email, split the steps and name the intermediate values.

Core forms are the small hand tools of the language. Literals are raw materials, calls ask for work, operators join values, ranges mark spans, and lambdas carry a small action around. Each form should feel boring before you rely on it inside larger data or effect code.

When a core form feels hard, place it in a plain story. A ticket price plus a fee, a room number in a range, a message sent through a callable, or a small action saved as a lambda is enough. The syntax matters because it keeps that story exact.
