.PHONY: check lint fmt test

# `crates_new/` is the canonical workspace. `crates/` is legacy reference-only.

check:
	cargo check --workspace && cargo check --workspace --tests

lint:
	cargo clippy --workspace --tests -- -D warnings

fmt:
	cargo fmt --all

test:
	cargo test --workspace
