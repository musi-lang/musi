---
title: "Code Organization"
description: "Move from single files into packages, imports, and exports."
group: "Code Organization"
section: "Code Organization"
order: 4
slug: "organization"
summary: "Grow from one file to packages without changing the mental model of code flow."
---

Organization begins when one file stops being enough. The language does not change at that moment, but the reading problem does. A reader now needs to know which file owns which idea, which names cross file boundaries, and which settings make the project repeatable.

Files are the first boundary. A file should have one main reason to exist: invoices, addresses, animal records, command-line entry, or runtime helpers. Packages are the larger boundary. A package says these files ship together, share configuration, and form one project.

Imports and exports are the hallway between rooms. Imports show what a file needs from outside. Exports show what the file promises to other files. That promise matters. Once a helper is exported, it stops being private craft and becomes part of the package's public face.

This part is intentionally practical. It is about keeping a codebase navigable when examples turn into projects. A small recipe app may start as one file; later it may split ingredients, lists, and meal plans. A payment tool may split card validation, receipt format, and audit logging. The split should help readers find the right room.

By the end, you should know when to add a file, when to keep a helper private, and when a package setting belongs in `musi.json` instead of one person's editor.
