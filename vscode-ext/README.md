# Musi for Visual Studio Code

Musi language support for VS Code: syntax highlighting, language-server features, formatting, and package commands for `.ms` projects.

## Features

- Syntax highlighting for `.ms` files and Musi fenced code blocks in Markdown
- Diagnostics, hover, semantic highlighting, formatting, and inlay hints from `musi_lsp`
- Format Document support for Musi source files
- CLI-backed formatting for Musi Markdown fences
- Package commands for run, check, build, test, format, and workspace actions
- `musi.json` schema validation
- Check-on-save fallback when LSP diagnostics are unavailable
- Named run configurations from VS Code settings

## Requirements

Install the Musi tools and make them available on PATH:

- `musi` for package commands, fallback checks, and Markdown fence formatting
- `musi_lsp` for editor diagnostics, hover, semantic tokens, formatting, and inlay hints

If the binaries are not on PATH, configure:

```json
{
  "musi.cliPath": "/path/to/musi",
  "musi.lspPath": "/path/to/musi_lsp"
}
```

When working inside the Musi repository, the extension also checks workspace build outputs before falling back to Cargo bin and PATH.

## Package detection

The extension treats the closest ancestor `musi.json` as the owning package root. Package commands, run configurations, task execution, and check-on-save use that package root.

Files without an owning `musi.json` still get syntax highlighting. Package commands and package checks stay disabled until the file belongs to a package.

## Commands

Open the command palette and run:

- `Musi: Run Package Entry`
- `Musi: Check Package`
- `Musi: Build Package`
- `Musi: Run Package Tests`
- `Musi: Format Document`
- `Musi: Format Workspace`
- `Musi: Check Workspace`
- `Musi: Build Workspace`
- `Musi: Run Workspace Tests`
- `Musi: Show Actions`
- `Musi: Start LSP`
- `Musi: Stop LSP`
- `Musi: Restart LSP`
- `Musi: Show LSP Output`

## Formatting

For `.ms` files, Format Document uses `musi_lsp` when the language server is running. If LSP formatting is unavailable, the extension falls back to `musi fmt`.

For Markdown files, Musi fenced code blocks are formatted with `musi fmt`.

Formatter behavior comes from the owning package's `musi.json` plus VS Code formatting options where supported.

## Settings

Common settings:

```json
{
  "musi.cliPath": "musi",
  "musi.lspPath": "musi_lsp",
  "musi.lsp.enabled": true,
  "musi.checkOnSave": true,
  "musi.hover.maximumLength": 500,
  "musi.inlayHints.enabled": true,
  "musi.inlayHints.parameterNames.enabled": "none",
  "musi.inlayHints.variableTypes.enabled": false
}
```

Runtime and terminal settings:

```json
{
  "musi.runtime.args": [],
  "musi.runtime.env": {},
  "musi.runtime.envFile": "",
  "musi.runtime.cwd": "",
  "musi.terminal.clearBeforeRun": false,
  "musi.terminal.focusOnRun": true,
  "musi.terminal.reuseTerminal": true
}
```

Named run configuration example:

```json
{
  "musi.runConfigurations": [
    {
      "name": "dev",
      "entry": "index.ms",
      "runtimeArgs": ["--watch"],
      "env": {
        "MUSI_ENV": "dev"
      },
      "preLaunchTask": "build"
    }
  ]
}
```

## Markdown support

Musi code fences in Markdown use the `musi` or `ms` language tag:

````markdown
```musi
let answer : Int := 42;
```
````

The extension highlights these fences and formats them through `musi fmt`.

## Troubleshooting

If diagnostics, hover, semantic highlighting, or formatting do not appear:

1. Run `Musi: Show LSP Output`.
2. Confirm `musi_lsp` is installed or set `musi.lspPath`.
3. Confirm the file belongs to a package with an ancestor `musi.json`.
4. Run `Musi: Restart LSP` after changing binary paths.

If package commands fail:

1. Confirm `musi` is installed or set `musi.cliPath`.
2. Check the package root detected from the nearest `musi.json`.
3. Verify runtime settings such as `musi.runtime.cwd`, `musi.runtime.env`, and `musi.runtime.envFile`.

## License

[MIT](LICENSE)
