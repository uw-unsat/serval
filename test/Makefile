SERVAL_DIR              := ..
OLEVEL				    := 2
ARCH                    := riscv64
O                       := o.$(ARCH)
CONFIG_DRAM_START       := 0x80000000

NPROC                   = $(shell nproc 2> /dev/null)
ifeq ($(NPROC),)
NPROC                   = 4
endif

RACO_JOBS               := $(NPROC)
RACO_TIMEOUT            := 1200
RACO_TEST               := raco test --check-stderr --table --timeout $(RACO_TIMEOUT) --jobs $(RACO_JOBS)


# no built-in rules and variables
MAKEFLAGS       += --no-builtin-rules --no-builtin-variables

BASE_CFLAGS     += -ffreestanding
BASE_CFLAGS     += -fno-stack-protector
BASE_CFLAGS     += -fno-strict-aliasing
# make it simpler for symbolic execution to track PC
BASE_CFLAGS     += -fno-jump-tables
# no unaligned memory accesses
BASE_CFLAGS     += -mstrict-align
BASE_CFLAGS     += -g -O$(OLEVEL)
BASE_CFLAGS     += -Wall -MD -MP

CONFIG_CFLAGS   += -DCONFIG_DRAM_START=$(CONFIG_DRAM_START)

CFLAGS          += $(BASE_CFLAGS) $(CONFIG_CFLAGS)
CFLAGS          += -mcmodel=medany
# no floating point
CFLAGS          += -mabi=lp64
CFLAGS          += -ffunction-sections -fdata-sections
CFLAGS          += -fno-PIE
CFLAGS          += -I include
CFLAGS          += -march=rv64ima

USER_CFLAGS     += $(BASE_CFLAGS)
USER_CFLAGS     += -I include/uapi

LDFLAGS         += -nostdlib
LDFLAGS         += --gc-sections

UBSAN_CFLAGS    += -fsanitize=integer-divide-by-zero
UBSAN_CFLAGS    += -fsanitize=shift
UBSAN_CFLAGS    += -fsanitize=signed-integer-overflow

include ../scripts/Makefile.lib

SERVAL_LLVM             := racket $(SERVAL_DIR)/serval/bin/serval-llvm.rkt

RACKET_TEST_C_SRCS      := $(wildcard *.c)
RACKET_TEST_S_SRCS      := $(wildcard *.S)

all: check-racket check-riscv-tests

%.ll.rkt: %.ll
	$(QUIET_GEN)$(SERVAL_LLVM) < $< > $@~
	$(Q)mv $@~ $@

%.globals.rkt: %.elf
	$(Q)echo "#lang reader serval/lang/dwarf" > $@~
	$(QUIET_GEN)$(OBJDUMP) --dwarf=info $< >> $@~
	$(Q)mv $@~ $@

$(O)/%.S: %.c
	$(Q)$(MKDIR_P) $(@D)
	$(QUIET_CC)$(CC) -o $@ $(filter-out -g,$(CFLAGS)) -S $<

# include zeros for bss in the binary
%.bin: %.elf
	$(QUIET_GEN)$(OBJCOPY) -O binary --set-section-flags .bss=alloc,load,contents $< $@

# --prefix-addresses prints the complete address on each line
%.asm: %.elf
	$(QUIET_GEN)$(OBJDUMP) -M no-aliases --prefix-addresses -w -f -d -z --show-raw-insn "$<" > "$@"

# sort addresses for *.map.rkt
%.map: %.elf
	$(QUIET_GEN)$(NM) --print-size --numeric-sort "$<" > "$@"

%.bin.o: %.bin bbl/payload.S
	$(QUIET_CC)$(CC) -o $@ -c -mabi=lp64 -DBBL_PAYLOAD='"$<"' bbl/payload.S

%.bbl: %.bin.o $(wildcard bbl/*.o) bbl/libmachine.a bbl/libsoftfloat.a bbl/libutil.a
	$(QUIET_LD)$(LD) -o $@ $(LDFLAGS) -T bbl/bbl.lds $^

$(O)/%.lds: %.lds.S
	$(Q)$(MKDIR_P) $(@D)
	$(QUIET_GEN)$(CPP) -o $@ -P $(CFLAGS) $<

$(O)/%.o: %.S
	$(Q)$(MKDIR_P) $(@D)
	$(QUIET_CC)$(CC) -o $@ -c $(CFLAGS) $<

$(O)/%.o: %.c
	$(Q)$(MKDIR_P) $(@D)
	$(QUIET_CC)$(CC) -o $@ -c $(CFLAGS) -D__MODULE__='"$(basename $(notdir $<))"' $<

%.asm.rkt: %.asm
	$(QUIET_GEN)echo "#lang reader serval/riscv/objdump" > $@~ && \
		cat $< >> $@~
	$(Q)mv $@~ $@

%.map.rkt: %.map
	$(QUIET_GEN)echo "#lang reader serval/lang/nm" > $@~ && \
		cat $< >> $@~
	$(Q)mv "$@~" "$@"

$(O)/%.py.rkt: %.py $(PYTHON_ROSETTE)
	$(Q)$(MKDIR_P) $(@D)
	$(QUIET_GEN)$(PYTHON_ROSETTE) $< > $@~
	$(Q)mv $@~ $@

%/asm-offsets.rkt: %/asm-offsets.S
	$(QUIET_GEN)$(call gen-offsets-rkt) < $< > $@~
	$(Q)mv $@~ $@

$(O)/%.ebpf.bin: %.ebpf
	$(Q)$(MKDIR_P) $(@D)
	$(QUIET_GEN)$(UBPF_ASSEMBLER) $^ $@

$(O)/%.ebpf.rkt: $(O)/%.ebpf.bin $(EBPF_ROSETTE)
	$(Q)$(MKDIR_P) $(@D)
	$(QUIET_GEN)$(UBPF_DISASSEMBLER) $< - | $(EBPF_ROSETTE) - > $@

$(O)/%.ll: %.c
	$(Q)$(MKDIR_P) $(@D)
	$(QUIET_CC)$(LLVM_CC) -o $@ -mno-sse -S -emit-llvm -fno-discard-value-names $(UBSAN_CFLAGS) -Wno-unused-command-line-argument -I include $(filter-out -g,$(BASE_CFLAGS)) $(CONFIG_CFLAGS) -DCONFIG_VERIFICATION_LLVM -c $<

$(O)/%.elf: $(O)/%.o
	$(QUIET_LD)$(LD) -o $@ -e $(CONFIG_DRAM_START) -Ttext $(CONFIG_DRAM_START) $(filter-out --gc-sections,$(LDFLAGS)) $<

$(patsubst %.c,%.rkt,$(RACKET_TEST_C_SRCS)): | $(addprefix $(O)/,\
        $(patsubst %.c,%.ll.rkt,$(RACKET_TEST_C_SRCS)) \
        $(patsubst %.c,%.asm.rkt,$(RACKET_TEST_C_SRCS)) \
        $(patsubst %.c,%.map.rkt,$(RACKET_TEST_C_SRCS)) \
        $(patsubst %.c,%.globals.rkt,$(RACKET_TEST_C_SRCS)))

$(patsubst %.S,%.rkt,$(RACKET_TEST_S_SRCS)): | $(addprefix $(O)/,\
        $(patsubst %.S,%.asm.rkt,$(RACKET_TEST_S_SRCS)))

$(patsubst %.py,%.rkt,$(RACKET_TEST_PY_SRCS)): | $(addprefix $(O)/,\
        $(patsubst %.py,%.py.rkt,$(RACKET_TEST_PY_SRCS)))

check-racket: $(wildcard *.rkt)
	$(RACO_TEST) $^

check-racket-%: %.rkt
	$(RACO_TEST) $^

include riscv-tests/riscv-tests.mk

clean:
	rm -rfv "$(O)"

.PHONY: all check-racket clean

.PRECIOUS: %.ll.rkt %.S.rkt $(O)/%.ll $(O)/%.S $(O)/%.elf
