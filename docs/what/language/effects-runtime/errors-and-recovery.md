---
title: "Errors and Recovery"
description: "Separate absent values, recoverable failures, and outside requests."
group: "Effects and Runtime"
section: "Effects and Runtime"
order: 25
slug: "errors-and-recovery"
summary: "Use Option for absence, Result for explained failure, and effects for work that needs an outside answer."
---
Error handling becomes confusing when every unwanted outcome uses the same shape. A missing discount code, a declined payment, and a printer that is out of paper are not the same kind of problem. One is absence, one is a recoverable business failure, and one is outside work needing a response.

Musi code reads better when those stories stay separate. `Option` handles the empty shelf. `Result` handles work that can explain why it did not produce the wanted value. Effects handle requests that leave ordinary calculation and need a handler or runtime to answer.

{{snippet:chapter-errors-and-recovery}}

Read the example like a checkout counter. Looking up a coupon may return nothing; that is normal. Charging a card may fail and give a reason; the cashier needs the reason. Printing the receipt asks a machine outside the calculation; that request belongs at an effect boundary.

## Recovery means the caller has a useful next step

Recoverable failure is not just a softer crash. It means the caller can do something sensible: show a message, choose a fallback, ask for another card, retry later, or save a draft. If the caller has no meaningful choice, do not invent a complicated result shape just to sound safer.

The best recovery examples are mundane. A file path can be wrong. A badge number can be unknown. A delivery route can be full. A price import can contain bad text. Each case tells the caller what to do next.

## Do not hide outside work in local-looking helpers

A helper named `currentTime()` may look like a local calculation, but the time comes from the host. A helper named `printReceipt()` may look like string formatting, but printing talks to a device. Effects make that crossing visible.

When tests become hard, check whether outside work is hiding in plain sight. Pure calculation should be easy to call. Requests should be close to the code that handles or exposes them.

## Pick the smallest honest shape

Use `Option` for empty-but-normal. Use `Result` for failure-with-reason. Use effects for outside requests. Some real programs use all three in one flow, but each part still has its own job.
