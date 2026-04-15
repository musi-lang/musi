---
title: "Effects"
description: "Introduce effect vocabulary before using clauses or handlers."
group: "Effects and Runtime"
section: "Effects and Runtime"
order: 24
slug: "effects"
summary: "Understand effects as requests for work, not immediate hidden side effects."
---

An effect describes operations code may request from an outside capability. A request is visible in source so the boundary stays clear.

{{snippet:chapter-effects}}

## Request Model

Read the `effect` block as a menu of operations. Read `request console.readLine();` as code asking for one operation from that menu. Something else must eventually provide the answer.

## Service Boundary

A request is like a service bell at a counter: code asks, handler or host answers.

Continue to [Using](/learn/book/effects-runtime/handling/using).
