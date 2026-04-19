---
title: "Option and Result"
description: "Choose between an absent value and a recoverable success-or-failure value."
group: "Data"
section: "Data"
order: 16
slug: "option-and-result"
summary: "Use Option for expected absence and Result for work that can explain why it failed."
---
Some programs fail because something is broken. Many programs simply do not find a value. A library card may not exist yet. A seat may be full. A receipt may not have a discount. Treating all of those as crashes makes ordinary work sound dangerous.

`Option` is for expected absence. It says, "there may be a value here." `Result` is for a recoverable success or failure. It says, "the work either produced a value or produced a reason it could not." Those are different stories, so Musi keeps them in different shapes.

{{snippet:chapter-option-and-result}}

The first value is like a coat-check ticket that found a coat. The second value is like a coat-check ticket with no coat attached yet. The result values are different: one says the payment produced a receipt number, and one says the payment failed with a message.

## Option is not a quiet null

A quiet null makes every later line suspicious. Does this customer have an address? Did the search fail? Did somebody forget to fill a field? With `Option`, the type tells you that absence is part of the plan.

Use `someOf` when the value exists. Use `noneOf` when the empty case is expected. A pet shelter can have an optional adopter, a delivery can have an optional driver, and a room can have an optional booking. None of those cases should pretend to be a normal record with missing fields.

## Result carries the reason

Use `Result` when the caller needs a reason. A payment might fail because the card was declined. A parse might fail because the text was not a number. A file load might fail because the path was wrong. The reason matters because the caller can choose a message, retry, or ask the user for a different value.

Do not use `Result` only because a value is absent. If a search for an open table returns nothing, `Option` is usually enough. If the reservation service could not be reached, `Result` tells the caller what went wrong.

## How to choose

Ask a small question: would an empty answer be normal? If yes, use `Option`. Would the caller need to know why work failed? If yes, use `Result`. Did the code ask the outside world to do something? Then also read the effects chapters, because failure and outside work are related but not the same thing.
