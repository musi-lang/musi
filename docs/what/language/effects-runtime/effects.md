---
title: "Effects"
description: "Introduce effect vocabulary before using clauses or handlers."
group: "Effects and runtime"
section: "Effects and runtime"
order: 24
slug: "effects"
summary: "Understand effects as requests for work, not immediate hidden side effects."
---

{{snippet:chapter-effects}}

## In this chapter

An effect describes operations code may request, and `request` issues one of those requests.
This pair of snippets keeps model intentionally small: first define console capability, then request one read operation from it.
That is core effect story before handlers, runtime imports, or stdlib layering enter scene.

## Why it matters

Users often know hidden side effects from other languages, but Musi wants capability flow to stay visible.
If docs jump straight to handlers, the basic question "what is an effect?" never gets a clean answer.
This page should make one thing obvious: effectful code is asking for work that something else must eventually provide.

## Walk through it

Read effect block as declaration of available operations, not as immediate implementation.
Then read `request console.readln();` as explicit request made from code that depends on that capability.
When writing your own examples, keep one effect and one operation at first so the request model stays sharper than the surrounding syntax.

## Try it next

- Define one effect with one operation.
- Request that operation once.
- Explain in words what still needs to handle the request.

## Common mistake

Do not skip straight to full handlers before the "request for work" model feels stable.

## Next

Continue to [Using](/docs/language/effects-runtime/using) to make required capabilities visible in function signatures.
