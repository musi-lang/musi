---
title: "Foundation"
description: "Separate language foundation from runtime and stdlib layers."
group: "Effects and runtime"
section: "Effects and runtime"
order: 27
slug: "foundation"
summary: "Understand what belongs to musi:core before reaching for stdlib modules."
---

{{snippet:chapter-foundation}}

## What

`musi:core` is language foundation layer.
It names the lowest-level built-in surface that exists before you start reaching for runtime-backed modules or standard-library conveniences.
This page matters because "what is built in?" and "what comes from libraries?" are different questions.

## Why

Users get overwhelmed when docs mention effects, runtime, and stdlib as if they are one blurred toolbox.
A foundation page prevents that blur by giving core layer its own place in the model.
Once readers know what belongs to the base layer, later imports make more sense.

## How

Read `let Core := import "musi:core";` as explicit access to foundational language surface.
Then ask what kind of code needs this layer directly: mostly infrastructure, lower-level libraries, or explanation of system boundaries rather than ordinary app code.
When teaching or writing app code, prefer clearer higher-level modules unless you specifically need the foundational layer.

## Try it

- Import `musi:core` once.
- Note what kind of code would reach for it directly.
- Compare that role with a higher-level stdlib import.

## Common mistake

Do not assume foundational modules are where everyday application code should start by default.

## Next

Continue to [Runtime](/docs/language/effects-runtime/runtime) to see where host-backed capabilities enter the picture.
