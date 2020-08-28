
# Debugging setup (run make V=1 for verbosity)
V ?= 0
ifeq ($(V),0)
Q = @
CARGO_FLAGS += -q
endif


#Give an error when cargo is not found
ifeq "$(shell whereis -b cargo)" "cargo:"
$(error Cargo/Rust doesn't seem to be installed! Please install from: https://www.rust-lang.org/tools/install)
endif


test: build
	@echo "\tTEST test.rkt"
	$(Q)raco test test.rkt

build: o/rust_test.ll.rkt o/rust_test.globals.rkt o/rust_test.map.rkt

# compiles llvm asm (or bitcode) to racket functions
o/%.ll.rkt: o/%.ll
	@echo "\tSERVAL-LLVM $^"
	$(Q)racket ../../serval/bin/serval-llvm.rkt < $^ > $@~
	$(Q)mv $@~ $@

o/%.map.rkt: o/%.o
	@echo "\tSERVAL-NM $^"
	$(Q)echo "#lang reader serval/lang/nm" > $@~
	$(Q)nm --print-size --numeric-sort $^ >> $@~
	$(Q)mv $@~ $@

o/%.globals.rkt: o/%.o
	@echo "\tSERVAL-DWARF $^"
	$(Q)echo "#lang reader serval/lang/dwarf" > $@~
	$(Q)objdump --dwarf=info $^ >> $@~
	$(Q)mv $@~ $@

# Rust compilations (handled by cargo)
o/rust_test.o: target/release/librust_test.rlib | o
	$(Q)cp target/release/deps/rust_test-*.o $@

o/rust_test.ll: target/release/librust_test.rlib | o
	$(Q)cp target/release/deps/rust_test-*.ll $@

# We have to do clean to make sure .../rust_test-*.(o|ll) only matches one file
target/release/librust_test.rlib: Cargo.toml $(shell find src -type f)
	@echo "\tCARGO $^"
	$(Q)cargo clean && \
	cargo rustc $(CARGO_FLAGS) --release -- --emit llvm-ir,obj

# Directory for build artifacts
o:
	$(Q)mkdir -p o

clean:
	$(Q)cargo clean
	$(Q)rm -rf o

.PRECIOUS: o/%.o o/%.ll o/%.ll.rkt o/%.map.rkt o/%.globals.rkt
