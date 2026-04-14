# Musi website

Public website for Musi. Stack stays **Bun + Vite + React**. Content stays static-first and prerendered into `www/dist`.

## Local workflow

Run from repository root:

- `bun run dev`
- `bun run dev:prod`
- `bun run check`
- `bun run test`
- `bun run build`
- `bun run build:prod`
- `bun run verify:lang`

`bun run build` does three things:

1. generate site content from docs/snippet sources
2. run Vite production build
3. prerender every public route into static HTML

`bun run dev:prod` builds the production bundle and serves `www/dist` through preview mode so local checks reflect production asset shape instead of Vite dev-server modules.

`bun run verify:lang` rebuilds the prerendered site and verifies representative English and Japanese pages keep the expected top-level `<html lang="...">`.

For Lighthouse accessibility audits, use a clean browser context. Browser extensions that inject shadow DOM can add nested `<html>` nodes and create a false `html[lang]` failure even when the shipped document already has the correct `lang` attribute.

## Structure

- `src/pages/` - route surfaces for home, learn, install, community, and playground
- `src/layout/` - shared shell, header, and docs sidebar
- `src/ui/` - site-owned primitives: actions, surfaces, page headers, TOC, theme
- `src/content.tsx` - shared generated snippets and install data
- `src/docs.ts` + `src/generated-content.ts` - generated docs route inventory and rendered HTML
- `scripts/generate-content.ts` - content generation entry
- `scripts/prerender.ts` - static HTML output and sitemap generation

## Design direction

Design target: **calm institutional explicitness**.

Rules:

- calm, not blank
- explicit, not verbose
- denser guidance where users need reassurance
- strong signifiers for links, buttons, nav, and current location
- warm clay/copper brand family derived from Musi icon
- similar colors allowed when they improve contrast or hierarchy
- no glass, glow, fake metrics, oversized radius, or decorative dashboard tropes

Primary public paths on home:

1. Learn
2. Install
3. Playground
4. Community

## Accessibility baseline

Target: **WCAG 2.2 AA**.

Contributor checklist:

- keep semantic landmarks: skip link, header, nav, main, and aside where needed
- keep heading order valid per page
- every interactive control must be keyboard reachable and visibly focused
- do not rely on color alone for meaning
- keep contrast compliant in both light and dark themes
- buttons must look interactive; links must look like links
- support reduced motion
- preserve readable zoomed layouts and horizontal overflow handling for code/tables
- label icon-only or stateful controls with accessible names

## Theme and palette

Theme modes:

- `light`
- `dark`
- `system`

Brand anchors come from favicon/file icon family:

- upper-left: `#9E6663`
- upper-right: `#AD6D62`
- lower-left: `#D38F7B`
- lower-right: `#C98273`

Nearby colors are allowed if they fit the family and improve WCAG compliance.

## Cloudflare Pages

Cloudflare Pages should build this site from repository root.

Use these settings:

- Root directory: repository root
- Build command: `bun install --frozen-lockfile && bun run build`
- Build output directory: `www/dist`

Recommended Pages environment variables:

- `BUN_VERSION=1.3.12`
- `SKIP_DEPENDENCY_INSTALL=1`
