---
title: "Records"
description: "Use records for labeled data before mixing in arrays or pattern matching."
group: "Data"
section: "Data"
order: 12
slug: "records"
summary: "Build named-field values and access fields directly."
---
Records gather named fields into one value. They are the right tool when the parts matter independently: a person has a name and age, a package has a weight and destination, and a car has a make and mileage.

{{snippet:chapter-records}}

Field names let code stay readable as the value grows. A record with three fields can become a record with five fields without asking every reader to remember positions.

## Records describe ordinary objects

Use records for things people would describe with a form or label. A shipping address, animal profile, payment receipt, and recipe ingredient all have named pieces that can be checked separately.

## Updating without losing meaning

When creating a changed copy, keep the unchanged fields visible through the record update form. This reads like changing the phone number on a contact card rather than rebuilding the whole person from memory.

Data chapters teach the shape of a room before anyone moves through it. A record is a labeled card, an array is an ordered row, a variant is a set of named doors, and a pattern is a safe way to open one door at a time.

Most data bugs start when one shape is asked to mean too many things. If a value can be absent, use an absent-value shape. If a value has named parts, use a record. If a value can be one of several cases, use data variants.
