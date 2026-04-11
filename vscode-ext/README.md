# Musi for Visual Studio Code

This extension is the pre-LSP Musi surface:

- TextMate syntax highlighting for `.ms` files and fenced Markdown blocks
- nearest-ancestor `musi.json` package ownership
- package-root commands for `musi run`, `musi check`, `musi build`, and `musi test`
- task execution from `musi.json`, including dependency-aware task plans
- automatic diagnostics from structured CLI output on save

Package root means the closest ancestor `musi.json`, not a hardcoded `packages/` folder. Files without an owning `musi.json` still get syntax support, but package commands and check-on-save stay disabled.

The extension does not ship fake pre-LSP surfaces. It does not register semantic tokens, inlay hints, or a debug adapter. Diagnostics come from `musi check --diagnostics-format json`, and package commands run through the configured `musi` CLI path.

Settings:

- `musi.cliPath`
- `musi.checkOnSave`
- `musi.runtime.*`
- `musi.terminal.*`
- `musi.runConfigurations`
