---
title: "Effects and Runtime"
description: "Understand effects, using, handlers, foundation, runtime, and stdlib layering."
group: "Effects and Runtime"
section: "Effects and Runtime"
order: 7
slug: "effects-runtime"
summary: "Make effect flow explicit, then place runtime and stdlib on top of that model."
---

Effects and Runtime explain how Musi talks to the world outside plain value calculation. A function can add numbers or format a record by itself. It cannot know the current time, read a file, print a line, or ask for randomness without crossing a boundary.

Effects make those boundaries visible. An effect declares operations that may be requested. A request asks for one of those operations. A handler or runtime answers it. Errors and Recovery sits near the front because absence, recoverable failure, and outside requests are easy to confuse. Keeping them separate makes ordinary business rules easier to test.

Use everyday examples while reading this part. A receipt total is ordinary calculation. Print the receipt is outside work. Choose a delivery window from a known date is ordinary calculation. Read today's date is outside work. Parse text is ordinary calculation. Read the file that contains the text is outside work.

The runtime chapters show the host-facing side: process data, file access, logging, time, randomness, text helpers, and path handling. The stdlib chapter explains shared vocabulary so every package does not rebuild its own option, result, or text helpers.

By the end, you should know where a request is made, who answers it, and how to move back to ordinary values as quickly as possible. That habit makes code easier to test, format, highlight, and review.
