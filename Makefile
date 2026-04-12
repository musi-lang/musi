.PHONY: check lint fmt test audit_coverage rscheck

check:
	cargo check --workspace && cargo check --workspace --tests

rscheck:
	@command -v rscheck >/dev/null 2>&1 || { \
		echo "rscheck not installed; run: cargo install --git https://github.com/xsyetopz/rscheck --locked rscheck-cli"; \
		exit 1; \
	}
	@PATH="$$HOME/.cargo/bin:$$PATH" rscheck check; code=$$?; test $$code -le 1

lint:
	$(MAKE) rscheck
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
