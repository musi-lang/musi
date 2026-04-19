---
title: "Abstractions"
description: "Learn classes, instances, and laws without object-model confusion."
group: "Abstractions"
section: "Abstractions"
order: 6
slug: "abstractions"
summary: "Separate behavior shape, concrete implementation, and semantic law into distinct chapters."
---

Abstractions are where Musi talks about shared behavior. A class describes a capability, an instance gives that capability to a specific type, and a law records the rules that implementations should obey.

This is easier to learn with ordinary examples. Several values can be compared: ages, delivery times, invoice totals, and priority levels. Several values can be shown as text: people, animals, tickets, and receipts. The shared behavior is real, even though each type stores different fields.

A class should name one coherent ability. If the methods do not belong in the same sentence, the class is probably too large. An instance should make one type satisfy that ability without smuggling in unrelated policy. A law should state a small rule that generic code can rely on.

Think of laws like the rules of a board game. Different players can choose different moves, but every move must still obey the game. Generic code has the same need. A sorting helper can work across many types only if comparison behaves consistently.

By the end of this part, you should be able to separate three questions: what behavior is required, how this type provides it, and what promises must remain true for generic code to trust it.
