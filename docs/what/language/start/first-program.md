---
title: "First Program"
description: "Create the smallest useful Musi file and run it with the direct command lane."
group: "Start"
section: "Start"
order: 2
slug: "first-program"
summary: "Write one file, bind one value, and run it end to end."
---
A first Musi program should show the shape of a real file: import what it needs, name a value, and export the part another file or tool can call. The goal is not to memorize every form. The goal is to see that a Musi file is mostly a list of declarations that can be read from top to bottom.

{{snippet:chapter-first-program}}

The example is small on purpose. It has the same bones as a larger program that prints a shipping label, prepares a pet feeding schedule, or checks whether a meeting room is free. There are names for data, there are expressions that make new values, and there is one exported entry point.

## What to notice first

Look for `let` before looking for clever syntax. `let` introduces a name, and the rest of the line tells you what value that name receives. When a declaration is exported, it becomes part of the file's public face. When it is not exported, it is a helper for this file.

## Growing without losing the thread

The next useful change is usually another named helper, not a larger expression. If a line starts to describe three ideas at once, give one idea a name. A receipt formatter might split `subtotal`, `tax`, and `message`; a game rule might split `canMove`, `nextSquare`, and `winner`.
