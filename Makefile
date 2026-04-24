.PHONY: check lint fmt test rscheck bench-vm bench-musi bench-csharp bench-csharp-smoke bench-csharp-vm-mode bench-csharp-smoke-vm-mode bench-fsharp bench-fsharp-smoke bench-fsharp-vm-mode bench-fsharp-smoke-vm-mode bench-java bench-java-smoke bench-java-vm-mode bench-java-smoke-vm-mode bench-scala bench-scala-smoke bench-scala-vm-mode bench-scala-smoke-vm-mode bench-vms bench-vms-smoke bench-vms-quick bench-vms-long bench-vms-gc bench-vms-peers install-local

RUST_TOOLCHAIN := 1.95.0
CARGO := rustup run $(RUST_TOOLCHAIN) cargo
JAVA17_HOME ?= /opt/homebrew/opt/openjdk@17
JAVA17_BIN := $(JAVA17_HOME)/bin
JAVA_BENCH_OUT := target/bench-java
SCALA_CACHE_DIR := target/bench-scala-cache
VM_BENCH_PHASE ?= both
VM_BENCH_ROUNDS ?= 5
VM_BENCH_ITERATIONS ?= 100000
VM_BENCH_WARMUP_ITERATIONS ?= 10000
VM_BENCH_WORKLOAD ?= all
VM_BENCH_ARGS := --phase $(VM_BENCH_PHASE) --rounds $(VM_BENCH_ROUNDS) --iterations $(VM_BENCH_ITERATIONS) --warmup-iterations $(VM_BENCH_WARMUP_ITERATIONS) --workload $(VM_BENCH_WORKLOAD)
CLR_VM_MODE_ENV := COMPlus_TieredCompilation=0 COMPlus_TC_QuickJit=0 COMPlus_TC_QuickJitForLoops=0 COMPlus_ReadyToRun=0 DOTNET_TieredPGO=0

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
	dotnet run -c Release --project benches/vm/csharp/MusiVmCSharpBenchmarks.csproj -- $(VM_BENCH_ARGS) --profile native

bench-csharp-smoke:
	dotnet run -c Release --project benches/vm/csharp/MusiVmCSharpBenchmarks.csproj -- --smoke --profile native

bench-csharp-vm-mode:
	$(CLR_VM_MODE_ENV) dotnet run -c Release --project benches/vm/csharp/MusiVmCSharpBenchmarks.csproj -- $(VM_BENCH_ARGS) --profile vm_mode

bench-csharp-smoke-vm-mode:
	$(CLR_VM_MODE_ENV) dotnet run -c Release --project benches/vm/csharp/MusiVmCSharpBenchmarks.csproj -- --smoke --profile vm_mode

bench-fsharp:
	dotnet run -c Release --project benches/vm/fsharp/MusiVmFSharpBenchmarks.fsproj -- $(VM_BENCH_ARGS) --profile native

bench-fsharp-smoke:
	dotnet run -c Release --project benches/vm/fsharp/MusiVmFSharpBenchmarks.fsproj -- --smoke --profile native

bench-fsharp-vm-mode:
	$(CLR_VM_MODE_ENV) dotnet run -c Release --project benches/vm/fsharp/MusiVmFSharpBenchmarks.fsproj -- $(VM_BENCH_ARGS) --profile vm_mode

bench-fsharp-smoke-vm-mode:
	$(CLR_VM_MODE_ENV) dotnet run -c Release --project benches/vm/fsharp/MusiVmFSharpBenchmarks.fsproj -- --smoke --profile vm_mode

bench-java:
	mkdir -p $(JAVA_BENCH_OUT)
	$(JAVA17_BIN)/javac -d $(JAVA_BENCH_OUT) benches/vm/java/Program.java
	$(JAVA17_BIN)/java -cp $(JAVA_BENCH_OUT) Program $(VM_BENCH_ARGS) --profile native

bench-java-smoke:
	mkdir -p $(JAVA_BENCH_OUT)
	$(JAVA17_BIN)/javac -d $(JAVA_BENCH_OUT) benches/vm/java/Program.java
	$(JAVA17_BIN)/java -cp $(JAVA_BENCH_OUT) Program --smoke --profile native

bench-java-vm-mode:
	mkdir -p $(JAVA_BENCH_OUT)
	$(JAVA17_BIN)/javac -d $(JAVA_BENCH_OUT) benches/vm/java/Program.java
	$(JAVA17_BIN)/java -Xint -cp $(JAVA_BENCH_OUT) Program $(VM_BENCH_ARGS) --profile vm_mode

bench-java-smoke-vm-mode:
	mkdir -p $(JAVA_BENCH_OUT)
	$(JAVA17_BIN)/javac -d $(JAVA_BENCH_OUT) benches/vm/java/Program.java
	$(JAVA17_BIN)/java -Xint -cp $(JAVA_BENCH_OUT) Program --smoke --profile vm_mode

bench-scala:
	mkdir -p $(SCALA_CACHE_DIR)
	JAVA_HOME=$(JAVA17_HOME) COURSIER_CACHE=$(CURDIR)/$(SCALA_CACHE_DIR)/coursier SCALA_CLI_HOME=$(CURDIR)/$(SCALA_CACHE_DIR)/scala-cli scala run --server=false benches/vm/scala/Program.scala -- $(VM_BENCH_ARGS) --profile native

bench-scala-smoke:
	mkdir -p $(SCALA_CACHE_DIR)
	JAVA_HOME=$(JAVA17_HOME) COURSIER_CACHE=$(CURDIR)/$(SCALA_CACHE_DIR)/coursier SCALA_CLI_HOME=$(CURDIR)/$(SCALA_CACHE_DIR)/scala-cli scala run --server=false benches/vm/scala/Program.scala -- --smoke --profile native

bench-scala-vm-mode:
	mkdir -p $(SCALA_CACHE_DIR)
	JAVA_HOME=$(JAVA17_HOME) COURSIER_CACHE=$(CURDIR)/$(SCALA_CACHE_DIR)/coursier SCALA_CLI_HOME=$(CURDIR)/$(SCALA_CACHE_DIR)/scala-cli scala run --server=false --java-opt -Xint benches/vm/scala/Program.scala -- $(VM_BENCH_ARGS) --profile vm_mode

bench-scala-smoke-vm-mode:
	mkdir -p $(SCALA_CACHE_DIR)
	JAVA_HOME=$(JAVA17_HOME) COURSIER_CACHE=$(CURDIR)/$(SCALA_CACHE_DIR)/coursier SCALA_CLI_HOME=$(CURDIR)/$(SCALA_CACHE_DIR)/scala-cli scala run --server=false --java-opt -Xint benches/vm/scala/Program.scala -- --smoke --profile vm_mode

bench-vms-peers:
	$(MAKE) bench-java
	$(MAKE) bench-java-vm-mode
	$(MAKE) bench-scala
	$(MAKE) bench-scala-vm-mode
	$(MAKE) bench-csharp
	$(MAKE) bench-csharp-vm-mode
	$(MAKE) bench-fsharp
	$(MAKE) bench-fsharp-vm-mode

bench-vms:
	$(MAKE) VM_BENCH_PHASE=both VM_BENCH_ROUNDS=5 VM_BENCH_ITERATIONS=100000 VM_BENCH_WARMUP_ITERATIONS=10000 bench-vms-peers
	$(MAKE) bench-vm

bench-vms-smoke:
	$(MAKE) bench-java-smoke
	$(MAKE) bench-java-smoke-vm-mode
	$(MAKE) bench-scala-smoke
	$(MAKE) bench-scala-smoke-vm-mode
	$(MAKE) bench-csharp-smoke
	$(MAKE) bench-csharp-smoke-vm-mode
	$(MAKE) bench-fsharp-smoke
	$(MAKE) bench-fsharp-smoke-vm-mode

bench-vms-quick:
	$(MAKE) VM_BENCH_PHASE=hot VM_BENCH_ROUNDS=3 VM_BENCH_ITERATIONS=50000 VM_BENCH_WARMUP_ITERATIONS=5000 bench-vms-peers
	$(MAKE) bench-vm

bench-vms-long:
	$(MAKE) VM_BENCH_PHASE=both VM_BENCH_ROUNDS=8 VM_BENCH_ITERATIONS=250000 VM_BENCH_WARMUP_ITERATIONS=25000 bench-vms-peers
	$(MAKE) bench-vm

bench-vms-gc:
	$(MAKE) VM_BENCH_PHASE=hot VM_BENCH_ROUNDS=3 VM_BENCH_ITERATIONS=2000 VM_BENCH_WARMUP_ITERATIONS=100 VM_BENCH_WORKLOAD=sequence_return_alloc bench-vms-peers
	$(MAKE) VM_BENCH_PHASE=hot VM_BENCH_ROUNDS=3 VM_BENCH_ITERATIONS=50 VM_BENCH_WARMUP_ITERATIONS=5 VM_BENCH_WORKLOAD=sequence_return_forced_gc bench-vms-peers
	$(MAKE) bench-vm

install-local:
	$(CARGO) build
	$(CARGO) build --release
	$(CARGO) install --locked --force --path crates/music
	$(CARGO) install --locked --force --path crates/musi
