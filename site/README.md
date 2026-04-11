# Website Hosting

This website is built as a Bun workspace rooted in `site/`.

## Cloudflare Pages

Use Cloudflare Pages Git integration against `musi-lang/musi`.

- Production branch: `main`
- Root directory: `site`
- Framework preset: `None`
- Build command: `bun install --frozen-lockfile && bun run build`
- Build output directory: `apps/www/dist`

## Pages Environment Variables

- `BUN_VERSION=1.3.12`
- `SKIP_DEPENDENCY_INSTALL=1`

`SKIP_DEPENDENCY_INSTALL=1` is required because the build command already performs the Bun install step inside the `site/` workspace root.

## Custom Domains

- `musi-lang.com` as the primary domain
- `www.musi-lang.com` as the secondary domain with redirect or canonical policy to the primary domain

## Local Verification

From `site/`:

```bash
bun run build
```

Expected static output:

```text
apps/www/dist
```
