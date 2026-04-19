---
title: "Exceptions and Effects"
description: "Read Exceptions and Effects as a Java habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Java Developers"
order: 8
slug: "exceptions-effects"
summary: "Translate the Java habit, then use the Musi Book for the full rule."
---

Java exceptions and services can make outside work appear as a normal method call. Musi effect code marks the request so runtime boundaries stay visible.

{{compare:java-exceptions-effects}}

## Reading Exceptions and Effects from Java

On the Musi side, Musi names outside work with effects and `request`, instead of hiding time, files, console work, or services inside an ordinary-looking call. Read the shared example through Java eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not make an effect look pure. A clock, process, network service, or console changes the trust boundary. For a Java reader, the trap is reading Musi `class` as a nominal object type with constructors and fields; Musi `class` is a behavior contract supplied by instances; records and data model object shape.

## When this pays off

Use effects when the program asks the runtime, OS, user, or another service for an answer. The Java instinct still helps here: Keep the Java habit of naming APIs for future readers.

## Keep close

- [Effects](/learn/book/effects-runtime/effects)
- [Using](/learn/book/effects-runtime/using)
- [Handlers](/learn/book/effects-runtime/handlers)
