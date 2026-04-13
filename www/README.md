# Musi website

Public website for Musi. Stack stays **Bun + Vite + React**. Content stays static-first and prerendered into `www/dist`.

## Local workflow

Run from repository root:

- `bun run dev`
- `bun run check`
- `bun run test`
- `bun run build`

`bun run build` does three things:

1. generate site content from docs/snippet sources
2. run Vite production build
3. prerender every public route into static HTML

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
5. Community guestbook

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
- `VITE_TURNSTILE_SITE_KEY`

## Guestbook on Cloudflare Pages

Guestbook runs through Pages Functions + D1.

- Function entry: `functions/api/guestbook.ts`
- D1 schema: `db/migrations/0001_guestbook.sql`
- Wrangler config: `wrangler.toml`
- Client env examples: `www/.env.example`
- Wrangler secret example: `.dev.vars.example`

Required setup:

1. Copy `www/.env.example` to `www/.env.local` for local work.
2. Copy `www/.env.example` to `www/.env.production` for production build defaults.
3. Copy `.dev.vars.example` to `.dev.vars` for local Pages Functions secrets.
4. Create D1 database with `wrangler d1 create musi-guestbook`.
5. Paste returned `database_id` into `wrangler.toml`.
6. Run migrations:
   - `wrangler d1 migrations apply musi-guestbook --local`
   - `wrangler d1 migrations apply musi-guestbook --remote`
7. Set `TURNSTILE_SECRET` in `.dev.vars` and Cloudflare Pages project secrets.
8. Set `VITE_TURNSTILE_SITE_KEY` in local env files and Cloudflare Pages build environment variables.

Local guestbook dev:

- build site: `bun run build`
- run Pages locally: `wrangler pages dev www/dist`

Behavior:

- when D1 + Turnstile secret + site key are present, Community shows guestbook submission form
- when submission config is incomplete, Community stays read-only and still shows entries
