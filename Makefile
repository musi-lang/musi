.PHONY: check lint fmt test audit_coverage rscheck

RUST_TOOLCHAIN := 1.95.0
CARGO := rustup run $(RUST_TOOLCHAIN) cargo

check:
	$(CARGO) check --workspace
	$(CARGO) check --workspace --tests

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
	$(CARGO) clippy --locked --workspace -- -D warnings
	$(CARGO) clippy --locked --workspace --tests -- -D warnings

fmt:
	$(CARGO) fmt --all

test:
	$(CARGO) test --workspace

audit_coverage:
	bash scripts/loc_crates.sh crates
	bash scripts/loc_guard.sh crates
	$(CARGO) test --workspace
	node vscode-ext/scripts/verify-grammars.mjs
