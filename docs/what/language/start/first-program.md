---
title: "First Program"
description: "Create the smallest useful Musi file and run it with the direct command lane."
group: "Start"
section: "Start"
order: 2
slug: "first-program"
summary: "Write one file, bind one value, and run it end to end."
---

A Musi program can be small enough to read in one breath: bind a value, end with a value, and let the tool check the file.

{{snippet:chapter-first-program}}

Read the semicolon after `let answer := 42;` as "this statement is done." Read the final `answer;` as the value the file leaves behind. That final-expression habit appears everywhere in Musi.

C-like languages often teach a program as a function that returns. Musi starts smaller: an expression can already be the result. Functions come next, once naming a reusable action matters.

## Reading a File

Read top to bottom. Names become available after their `let`. Later expressions use earlier names. There is no hidden global setup in this example, and there is no ordinary `return` keyword at the end.

Continue to [Values and Let](/learn/book/start/foundations/values-and-let).
