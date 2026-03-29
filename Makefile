.PHONY: check lint fmt

SCOPE ?= new
VALID_SCOPES := new all legacy
NEW_PACKAGES := music_basic music_storage music_names music_lex music_il music_assembly
LEGACY_PACKAGES := $(shell find crates -mindepth 2 -maxdepth 2 -name Cargo.toml -exec sed -n 's/^name = "\(.*\)"/\1/p' {} \;)

ifeq ($(filter $(SCOPE),$(VALID_SCOPES)),)
$(error Unsupported SCOPE '$(SCOPE)'. Use one of: $(VALID_SCOPES))
endif

ifeq ($(SCOPE),all)
PACKAGE_FLAGS :=
FMT_FLAGS := --all
else ifeq ($(SCOPE),legacy)
PACKAGE_FLAGS := $(foreach pkg,$(LEGACY_PACKAGES),-p $(pkg))
FMT_FLAGS := $(PACKAGE_FLAGS)
else
PACKAGE_FLAGS := $(foreach pkg,$(NEW_PACKAGES),-p $(pkg))
FMT_FLAGS := $(PACKAGE_FLAGS)
endif

check:
	cargo check $(PACKAGE_FLAGS) && cargo check --tests $(PACKAGE_FLAGS)

lint:
	cargo clippy $(PACKAGE_FLAGS) && cargo clippy --tests $(PACKAGE_FLAGS)

fmt:
	cargo fmt $(FMT_FLAGS)
