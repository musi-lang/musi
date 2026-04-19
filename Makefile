.PHONY: check lint fmt test rscheck bench-vm bench-musi bench-csharp bench-csharp-smoke bench-fsharp bench-fsharp-smoke bench-java bench-java-smoke bench-scala bench-scala-smoke bench-vms bench-vms-smoke install-local

RUST_TOOLCHAIN := 1.95.0
CARGO := rustup run $(RUST_TOOLCHAIN) cargo
JAVA17_HOME ?= /opt/homebrew/opt/openjdk@17
JAVA17_BIN := $(JAVA17_HOME)/bin
JAVA_BENCH_OUT := target/bench-java
SCALA_CACHE_DIR := target/bench-scala-cache

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

bench-vm bench-musi:
	$(CARGO) bench -p musi_vm --bench bench_vm -- --noplot

bench-csharp:
	dotnet run -c Release --project benches/vm/csharp/MusiVmCSharpBenchmarks.csproj

bench-csharp-smoke:
	dotnet run -c Release --project benches/vm/csharp/MusiVmCSharpBenchmarks.csproj -- --smoke

bench-fsharp:
	dotnet run -c Release --project benches/vm/fsharp/MusiVmFSharpBenchmarks.fsproj

bench-fsharp-smoke:
	dotnet run -c Release --project benches/vm/fsharp/MusiVmFSharpBenchmarks.fsproj -- --smoke

bench-java:
	mkdir -p $(JAVA_BENCH_OUT)
	$(JAVA17_BIN)/javac -d $(JAVA_BENCH_OUT) benches/vm/java/Program.java
	$(JAVA17_BIN)/java -cp $(JAVA_BENCH_OUT) Program

bench-java-smoke:
	mkdir -p $(JAVA_BENCH_OUT)
	$(JAVA17_BIN)/javac -d $(JAVA_BENCH_OUT) benches/vm/java/Program.java
	$(JAVA17_BIN)/java -cp $(JAVA_BENCH_OUT) Program --smoke

bench-scala:
	mkdir -p $(SCALA_CACHE_DIR)
	JAVA_HOME=$(JAVA17_HOME) COURSIER_CACHE=$(CURDIR)/$(SCALA_CACHE_DIR)/coursier SCALA_CLI_HOME=$(CURDIR)/$(SCALA_CACHE_DIR)/scala-cli scala run --server=false benches/vm/scala/Program.scala

bench-scala-smoke:
	mkdir -p $(SCALA_CACHE_DIR)
	JAVA_HOME=$(JAVA17_HOME) COURSIER_CACHE=$(CURDIR)/$(SCALA_CACHE_DIR)/coursier SCALA_CLI_HOME=$(CURDIR)/$(SCALA_CACHE_DIR)/scala-cli scala run --server=false benches/vm/scala/Program.scala -- --smoke

bench-vms:
	$(MAKE) bench-java
	$(MAKE) bench-scala
	$(MAKE) bench-csharp
	$(MAKE) bench-fsharp
	$(MAKE) bench-vm

bench-vms-smoke:
	$(MAKE) bench-java-smoke
	$(MAKE) bench-scala-smoke
	$(MAKE) bench-csharp-smoke
	$(MAKE) bench-fsharp-smoke

install-local:
	$(CARGO) build
	$(CARGO) build --release
	$(CARGO) install --locked --force --path crates/music
	$(CARGO) install --locked --force --path crates/musi
