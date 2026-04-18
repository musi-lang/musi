.PHONY: check lint fmt test rscheck

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
	$(CARGO) run -p musi_diaggen -- write
	$(CARGO) run -p musi_diaggen -- check
	$(MAKE) rscheck
	$(CARGO) clippy --locked --workspace -- -D warnings
	$(CARGO) clippy --locked --workspace --tests -- -D warnings

fmt:
	$(CARGO) fmt --all

test:
	$(CARGO) test --workspace
