# Musi for Visual Studio Code

This extension is the Musi editor surface:

- TextMate syntax highlighting for `.ms` files and fenced Markdown blocks
- nearest-ancestor `musi.json` package ownership
- `musi_lsp` diagnostics and hover for Musi source files
- package-root commands for `musi run`, `musi check`, `musi build`, and `musi test`
- task execution from `musi.json`, including dependency-aware task plans
- automatic CLI fallback diagnostics on save when LSP is unavailable, plus manifest checks

Doc comments use a JSDoc-like sublanguage, but with Musi syntax for tag values:

```musi
/// @parameter value := input value
/// @see {@linkplain Std.Option | Option} and https://example.com
/**
 * @type T := result type
 * @default [count := 10]
 * @optional [count := 10]
 * @code let value := 1
 */
let value := 1;
```

Package root means the closest ancestor `musi.json`, not a hardcoded `packages/` folder. Files without an owning `musi.json` still get syntax support, but package commands and check-on-save stay disabled.

The extension does not ship fake editor surfaces. It does not register semantic tokens, inlay hints, or a debug adapter. LSP features come from `musi_lsp`, and package commands run through the configured `musi` CLI path. CLI diagnostics remain as fallback transport and keep manifest checks active.

Settings:

- `musi.cliPath`
- `musi.lspPath`
- `musi.checkOnSave`
- `musi.runtime.*`
- `musi.terminal.*`
- `musi.runConfigurations`
