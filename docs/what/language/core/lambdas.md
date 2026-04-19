---
title: "Lambdas"
description: "Write a small function value directly inside an expression."
group: "Core Syntax"
section: "Core Syntax"
order: 12
slug: "lambdas"
summary: "Use lambda expressions when a short function value reads better in place."
---
A lambda is a small function written where it is used. It is useful when the action is local to one operation, like choosing the display name for each person in a list or deciding whether an order should be kept.

{{snippet:chapter-lambdas}}

Use a lambda when naming a separate function would add distance without adding understanding. If the same lambda appears twice, it has earned a real name.

## Local behavior

The best lambdas are brief and specific. A map over animals might turn each animal into a tag label. A filter over payments might keep only successful payments. The surrounding expression already explains the larger task.

## When to promote one

If a lambda needs comments, multiple branches, or several local declarations, it is no longer just a small inline action. Move it to a function so the name can carry the idea.

Core forms are the small hand tools of the language. Literals are raw materials, calls ask for work, operators join values, ranges mark spans, and lambdas carry a small action around. Each form should feel boring before you rely on it inside larger data or effect code.

When a core form feels hard, place it in a plain story. A ticket price plus a fee, a room number in a range, a message sent through a callable, or a small action saved as a lambda is enough. The syntax matters because it keeps that story exact.
