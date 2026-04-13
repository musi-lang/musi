# Musi Website

Cloudflare Pages should build this site from the repository root.

Use these Pages settings:

- Root directory: repository root
- Build command: `bun install --frozen-lockfile && bun run build`
- Build output directory: `www/dist`

Recommended Pages environment variables:

- `BUN_VERSION=1.3.12`
- `SKIP_DEPENDENCY_INSTALL=1`

Local commands from repo root:

- `bun run dev`
- `bun run check`
- `bun run test`
- `bun run build`

---

```
upper-left: #9E6663
upper-right: #AD6D62
lower-left: #D38F7B
lower-right: #C98273

---

upper-left_test: #639B9E
upper-right_test: #62A8AD
lower-left_test: #7BBFD3
lower-right_test: #73BAC9
```
