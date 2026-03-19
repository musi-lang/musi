.PHONY: check lint

check:
	cargo fmt --all && cargo check && cargo check --tests
lint:
	cargo fmt --all && cargo clippy && cargo clippy --tests
