---
title: "Getting Started"
description: "Install prerequisites, install Musi with script or Cargo, and learn when to use music versus musi."
group: "Start"
section: "Start"
order: 1
slug: "getting-started"
summary: "Install tools, install Musi, and learn the two command lanes."
---
Musi starts well as a small-file language. You can write one file, run it, and keep the whole program in your head. When the file becomes a package, the same habits still work: name the parts, import only what the file uses, and let `musi.json` describe the project instead of hiding settings in an editor.

Think of the first program like labeling boxes before moving house. A single box can be carried by hand. A room full of boxes needs labels, a path through the hallway, and a rule for where each box lands. Musi's command-line tools give you that path without changing the language underneath.

{{snippet:chapter-getting-started}}

The command you choose depends on the question you are asking. Use a direct run when you want to know whether a file still behaves. Use project commands when imports, generated output, package metadata, or formatter settings matter. In both cases, the useful habit is to keep source code ordinary and repeatable: a teammate should be able to check out the package and get the same answer.

## A tiny working loop

Start with one exported value or function, run it, then add the next piece. If the program is about a grocery list, do not start with a full store model. Start with `Item`, then `Cart`, then one function that totals a cart. Each step should leave a shape that can be named.

## Moving from file to package

A package becomes useful when the program has more than one reason to change. Payment code, address formatting, and invoice printing belong in separate files because they answer separate questions. Imports make those boundaries visible, and exports decide what the rest of the package may use.
