.PHONY: check lint fmt test audit_coverage

# `crates_new/` is the canonical workspace. `crates/` is legacy reference-only.

check:
	cargo check --workspace && cargo check --workspace --tests

lint:
	bash scripts/audit_paths.sh crates_new
	bash scripts/audit_god_crates.sh crates_new
	cargo clippy --workspace && cargo clippy --workspace --tests

fmt:
	cargo fmt --all

test:
	cargo test --workspace

audit_coverage:
	bash scripts/loc_crates.sh crates_new
	bash scripts/loc_guard.sh crates_new
	cargo test --workspace
	node vscode-ext/scripts/verify-grammars.mjs
