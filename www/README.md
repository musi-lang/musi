# Musi Website

Public website for Musi. The website is a static-first Bun + Astro + Vite + Preact app that builds into `www/dist`.

This README is the operations guide for local website contributors, docs authors, and the people publishing the main Musi website.

## Local Contributor

Run commands from the repository root unless a command says otherwise.

Install dependencies:

```bash
bun install
```

Start the local website:

```bash
bun run dev
```

Build and preview the production output:

```bash
bun run dev:prod
```

Validate before sending website or docs changes:

```bash
bun run check
bun run test
bun run verify:lang
```

What those commands cover:

- `bun run check`: regenerate docs content, validate language docs inputs, typecheck, and run Biome checks.
- `bun run test`: regenerate docs content, validate language docs inputs, and run Vitest tests for website code and content generation.
- `bun run verify:lang`: build the static site and verify every generated HTML page has `<html lang="en">`.

For Lighthouse accessibility audits, use a clean browser profile or incognito window with extensions disabled. Browser automation extensions can inject nodes such as `browser-mcp-container` or `data-mcp-root`; if DevTools shows those nodes, `html[lang]` warnings are audit-context noise, not shipped site markup.

## Docs Author

Human-facing language docs live under `docs/what/language`.
Website maintainer docs live under `docs/what/website`.
Book section landing pages live under `www/src/content/book/language`.

Docs content is repo-backed. Markdown carries authored prose, TypeScript files carry structure, and generated content is committed for the static site:

- Markdown files hold page copy.
- `www/src/content/book/manifest.ts` controls routing, grouping, aliases, and order.
- `www/src/content/snippet-registry.ts` and `www/src/content/examples/` provide code examples used by Markdown placeholders.
- `www/scripts/generate-content.ts` deterministically renders docs into `www/src/generated/rendered-docs.json` and `www/src/generated/rendered-snippets.json`.
- `www/scripts/validate-language-docs.ts` validates frontmatter, placeholder references, and syntax-fence rules before generation.

Rules for language docs:

- Teach current Musi syntax only.
- Do not describe syntax as old, new, former, or transitional.
- Do not use raw ````musi` fences in learning docs.
- Use snippet or example placeholders so examples are highlighted and checked by generation.
- Keep consumer-facing docs educational; compiler-internal notes belong under maintainer docs or crate docs.

## Local Docs Studio

Docs Studio is a local-only Markdown editor for the repository docs.
It is not deployed and does not need a database, external auth app, email provider, or hosted admin service.

Start it from the repository root:

```bash
bun run docs:studio
```

Then open:

```text
http://127.0.0.1:4322
```

Editable roots:

- `docs/what/language`
- `docs/what/website`
- `www/src/content/book/language`

Docs Studio edits the same repository files that authors review in Git.
Review changes before committing.
The editor rejects paths outside the allowed docs roots.

## Website Structure

- `src/pages/`: Astro routes for static pages, docs aliases, robots, sitemap, and 404.
- `src/site-pages/`: route surfaces for home, learn, install, community, and playground.
- `src/layout/`: shared shell, header, and docs sidebar.
- `src/ui/`: site-owned primitives such as actions, surfaces, page headers, TOC, and theme controls.
- `src/docs.ts` and `src/generated-content.ts`: generated docs route inventory and rendered HTML.
- `scripts/generate-content.ts`: deterministic content generation entrypoint.
- `scripts/validate-language-docs.ts`: language docs input validator used by generation.

Astro owns document HTML, metadata, static routing, sitemap, robots, and 404 output. Preact is reserved for static-compatible components and small local islands when interaction needs state.

## Design And Accessibility

Design target: calm institutional explicitness.

Rules:

- calm, not blank
- explicit, not verbose
- strong signifiers for links, buttons, navigation, and current location
- warm clay/copper brand family from the Musi icon
- supporting colors allowed when they improve contrast or hierarchy
- no glass, glow, fake metrics, oversized radius, or decorative dashboard tropes

Target WCAG 2.2 AA.

Contributor checklist:

- Keep semantic landmarks: skip link, header, nav, main, and aside where needed.
- Keep heading order valid per page.
- Make every interactive control keyboard reachable and visibly focused.
- Do not rely on color alone for meaning.
- Keep contrast compliant in light and dark themes.
- Make buttons look interactive and links look like links.
- Support reduced motion.
- Preserve readable zoomed layouts and horizontal overflow handling for code and tables.
- Label icon-only or stateful controls with accessible names.

## Publisher Maintainer

The public website deploys as a static Cloudflare Pages site from this repository.

Docs editing is local-only contributor tooling.
The public site must not depend on an admin service being live.

### Cloudflare Pages

Build this site from the repository root.

Use these settings:

- Root directory: repository root
- Build command: `bun install --frozen-lockfile && bun run build:prod`
- Build output directory: `www/dist`

Recommended Pages environment variables:

- `BUN_VERSION=1.3.12`
- `SKIP_DEPENDENCY_INSTALL=1`

These values are committed in root `wrangler.toml`. Keep dashboard values unset
unless overriding them for a deployment environment.

Recommended Cloudflare dashboard settings:

- disable managed `robots.txt` augmentation that injects directives such as `Content-Signal`
- disable Web Analytics / Browser Insights if Lighthouse should stay free of Cloudflare beacon warnings

Deploy verification:

```bash
curl -s https://musi-lang.org/ | head
curl -s https://musi-lang.org/robots.txt
```

Confirm live HTML matches `www/dist` and `robots.txt` contains only repo-generated directives.
