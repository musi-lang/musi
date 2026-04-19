---
title: "Data"
description: "Work with records, arrays, slices, and patterns one step at a time."
group: "Data"
section: "Data"
order: 3
slug: "data"
summary: "Keep data-shape learning readable by separating records, sequences, and patterns."
---

Data is where programs start to describe the real world. People have names, cars have mileage, payments have states, animals have tags, rooms have bookings, and packages have destinations. Musi gives you several ways to model those shapes, and each one answers a different question.

Records are for named fields. Use them when the parts deserve labels, like `street`, `city`, and `postalCode`. Arrays and slices are for ordered collections, like seats in a row or payments on a statement. Data definitions are for meaningful alternatives, like pending, paid, or failed. Patterns are for reading those shapes back safely. Option and Result explain the two most common special shapes: maybe there is no value, or maybe the work failed with a reason.

This part should make code less vague. A boolean can say whether something is active, but a variant can say whether a subscription is trialing, active, paused, cancelled, or expired. A tuple can hold two values, but a record can say which value is `lowerBound` and which is `upperBound`.

Good data modeling removes guesswork from later code. If the shape is honest, functions become shorter because they no longer need to check for impossible combinations. If the shape is vague, every function has to defend itself.

Read these chapters with everyday examples in mind. A lost-and-found desk, a school roster, a pet shelter, a delivery route, and a family calendar all have different data shapes. The language features matter because they let those shapes stay visible.
