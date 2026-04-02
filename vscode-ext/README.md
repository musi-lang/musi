# Musi Language Support for Visual Studio Code

This extension ships the CLI-driven tooling that works with the Musi runtime.

- `Run File` / `Check File` / `Build File` commands invoke `musi` from the configured path.
- Task commands that read `musi.json` make it easy to kick off project-level scripts.
- Debug and test helpers use the same runner infrastructure so nothing depends on a language server.
- TextMate highlighting is syntax-only fallback. Semantic token registration is wired separately so name meaning does not get hardcoded into the fallback grammar.

Use the VS Code settings under `musi.cliPath`, `musi.compiler.*`, and `musi.runConfigurations` to control how the CLI is launched.
