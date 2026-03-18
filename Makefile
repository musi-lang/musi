.PHONY: check lint build

check:
	cargo fmt --all && cargo check && cargo check --tests

lint:
	cargo fmt --all && cargo clippy && cargo clippy --tests

build:
    cargo fmt --all && cargo build && cargo build --release
