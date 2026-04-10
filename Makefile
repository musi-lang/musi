.PHONY: check lint fmt test audit_coverage

check:
	cargo check --workspace && cargo check --workspace --tests

lint:
	bash scripts/audit_paths.sh crates
	bash scripts/audit_god_crates.sh crates
	cargo clippy --workspace && cargo clippy --workspace --tests

fmt:
	cargo fmt --all

test:
	cargo test --workspace

audit_coverage:
	bash scripts/loc_crates.sh crates
	bash scripts/loc_guard.sh crates
	cargo test --workspace
	node vscode-ext/scripts/verify-grammars.mjs
