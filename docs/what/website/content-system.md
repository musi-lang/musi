---
title: Website Content System
---

# Website Content System

The website uses a repo-backed content system.
Markdown files stay in the repository, TypeScript manifests describe routing and grouping, snippets and examples carry checked code, and the generator emits the runtime content registry used by the static site.
Generation is deterministic: the same registry and markdown inputs always produce the same checked-in JSON artifacts.

## Collections

The content catalog separates public docs into collections:

- `book`: ordered Musi Book chapters.
- `guides`: Musi for Developers transition guides.
- `reference`: advanced and lookup-oriented pages.

Each entry has one canonical route, zero or more aliases, and one source file.
Aliases keep older links alive while canonical routes use `/learn/book/...`.

## Source Rules

Docs should teach current Musi syntax only.
Do not describe syntax as old, new, current, former, or transitional.
If a construct changed, write the page as if the current language always worked that way.

Musi examples should come from snippets or example groups so generation can validate and highlight them.
Raw `musi` fences are not allowed in learning docs.

## Writing Style

Use Title Case for standalone labels and page titles.
Use sentence case in paragraphs.
Use ASCII quotes.
Avoid repeated chapter shells such as "In this chapter", "Why it matters", "Walk through it", and "Try it next".
A chapter should explain the concept in the shape that best fits that concept.

## Visual System

The website keeps Musi's warm clay identity, but pages may use accessible blue, green, amber, and purple accents when they carry meaning.
Every palette change must preserve readable light and dark modes, visible focus states, and non-color cues for status or navigation.

## Editing Surface

Human-facing website copy lives in Markdown files under `docs/what/language`.
Website maintainer docs live in Markdown files under `docs/what/website`.
Book section landings live under `www/src/content/book/language`.

Local Docs Studio edits those Markdown roots from `http://127.0.0.1:4322`.
It is a local editor for repository files, with Git review as the publishing gate.
It rejects paths outside the allowed docs roots.
Run it with `bun run docs:studio`.

Before publishing docs changes:

- Run `bun run validate:language-docs` to validate frontmatter, placeholder references, and raw-fence rules.
- Run `bun run generate:content` to refresh deterministic generated artifacts.
