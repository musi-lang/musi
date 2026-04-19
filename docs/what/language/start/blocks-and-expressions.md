---
title: "Blocks and Expressions"
description: "Learn how blocks produce results, why Musi does not need `return`, and how to model repetition without loop statements."
group: "Start"
section: "Start"
order: 5
slug: "blocks-and-expressions"
summary: "Treat a block as one expression with setup at the top and the result at the bottom."
---
Blocks let several expressions travel together and produce one final answer. They are useful when a result needs a little preparation: clean the customer's name, compute a discount, then return the final receipt line.

{{snippet:chapter-blocks-and-expressions}}

The last expression is the value the block gives back. Earlier declarations are the scratch work. That scratch work still has normal names and normal types, so a block can be read like a short checklist instead of a hidden control-flow trick.

## Reading a block out loud

Read each line as a step toward the final value. In a pet-care app, a block might find the animal's age, choose the feeding plan, and return a message for the owner. The block does not become a new world; it is a small room where temporary names live.

## Keeping blocks small

When a block starts to contain multiple stories, split it. A block that validates a payment, formats an address, and writes an audit event is doing too much. Name the pieces and let each block finish one thought.

Read this chapter like a short errand. First find the names, then find the value each name holds, then ask what the last expression gives back. A cafe ticket, a room card, a pet tag, and a bus route all become easier when each useful fact has a clear name.

A common mistake is to race toward large examples. Stay small until the form feels normal. One file, one value, one block, and one changed value teach more than a page full of clever tricks.
