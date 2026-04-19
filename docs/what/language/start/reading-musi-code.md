---
title: "Reading Musi Code"
description: "Learn how to scan a Musi file before learning every rule in detail."
group: "Start"
section: "Start"
order: 3
slug: "reading-musi-code"
summary: "Read declarations, imports, final expressions, type notes, and effect requests in the order a beginner sees them."
---
Reading code is a separate skill from writing code. A beginner often opens a file and tries to understand every symbol at once. That is like walking into a kitchen during dinner service and staring at every pan, timer, receipt, and shelf label. Start with the labels that tell you where things belong.

A Musi file is usually a list of declarations. Imports say what other shelves this file reaches for. `let` gives a value a name. `export let` puts a name in the front window so another file can use it. Type annotations are labels on jars: they do not cook the food, but they tell you what is inside.

{{snippet:chapter-reading-musi-code}}

Read the example in layers. First, the file brings in `@std/option`, so it will talk about a value that may be present or absent. Next, `Receipt` names a shape from the domain. Then `emptyReceipt` says what absence looks like for this small story. Last, `receiptForTable` is exported because another file might ask for it.

The order matters less than the habit. Do not start by asking whether every feature is familiar. Ask: what names does this file define, which names are public, and where does outside help come from?

## Names before mechanics

Most Musi code becomes easier when you read names before punctuation. In `let room := 12;`, the important fact is not the semicolon. The important fact is that later code can say `room` instead of repeating the literal. In `export let receiptForTable`, the important fact is that `receiptForTable` belongs to the module surface.

A name should feel like a label on an object in the room. `driver`, `route`, `animal`, `ticket`, `price`, and `receipt` all tell the next reader what world the program is describing. If you see a vague name, slow down. The line may be correct, but the program is making you carry extra context in your head.

## Look for boundaries

Imports and effects are boundaries. An import says this file uses code from another place. An effect request says this expression needs an answer from outside ordinary calculation. A receipt total can be calculated locally. Reading the current time, printing the receipt, or reading the process environment crosses a boundary.

When a file is hard to read, mark the boundaries first. Which imports come from packages? Which names are exported? Which functions say `using`? Which lines use `request`? After that, the local declarations usually become much smaller.

## Read small programs like real errands

A program is often just an errand with names. Make a label for a parcel. Count animals in a shelter. Find the first open appointment. Choose the price for a ticket. Musi syntax matters because it keeps those errands honest, but the errand is what gives the code meaning.

If a page later shows records, variants, classes, or effects, use the same habit. Ask what real thing is being named and which impossible states the type is trying to remove.
