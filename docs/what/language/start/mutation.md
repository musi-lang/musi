---
title: "Mutation"
description: "Learn Musi's explicit mutation surface without mixing it into every lesson."
group: "Start"
section: "Start"
order: 6
slug: "mutation"
summary: "Use mut only when changing a value helps more than rebuilding it."
---
Most Musi examples are easier to understand when values do not change after they are named. Mutation is still useful when the real action is a sequence of updates: filling a bucket, counting people through a door, or building a list of warnings while checking a form.

{{snippet:chapter-mutation}}

Use mutation when the changing state is the point of the code. If the code is only calculating a new answer from old inputs, a fresh name is usually clearer than changing an old one.

## A good place for changing state

A loop that scans rows in a file may keep a running total. A simulation may move a car from one position to the next. A parser may advance through tokens. In those cases, the name represents a thing whose current state matters.

## A bad place for changing state

Do not mutate because a name was convenient earlier. If a value changes meaning halfway through a function, the reader has to remember the old story and the new story at the same time. Use a new name when the meaning changes.

Read this chapter like a short errand. First find the names, then find the value each name holds, then ask what the last expression gives back. A cafe ticket, a room card, a pet tag, and a bus route all become easier when each useful fact has a clear name.

A common mistake is to race toward large examples. Stay small until the form feels normal. One file, one value, one block, and one changed value teach more than a page full of clever tricks.
