.PHONY: check lint fmt test audit_coverage rscheck

check:
	cargo check --workspace && cargo check --workspace --tests

rscheck:
	@RSCHECK_BIN="$$(command -v rscheck 2>/dev/null || true)"; \
	if [ -z "$$RSCHECK_BIN" ] && [ -x "$$HOME/.cargo/bin/rscheck" ]; then \
		RSCHECK_BIN="$$HOME/.cargo/bin/rscheck"; \
	fi; \
	if [ -z "$$RSCHECK_BIN" ] || [ ! -x "$$RSCHECK_BIN" ]; then \
		echo "rscheck not installed; run: cargo install rscheck-cli --locked"; \
		exit 1; \
	fi; \
	"$$RSCHECK_BIN" check; code=$$?; test $$code -le 1

lint:
	$(MAKE) rscheck
	bash scripts/audit_god_crates.sh crates
	cargo clippy --locked --workspace -- -D warnings
	cargo clippy --locked --workspace --tests -- -D warnings

fmt:
	cargo fmt --all

test:
	cargo test --workspace

audit_coverage:
	bash scripts/loc_crates.sh crates
	bash scripts/loc_guard.sh crates
	cargo test --workspace
	node vscode-ext/scripts/verify-grammars.mjs
