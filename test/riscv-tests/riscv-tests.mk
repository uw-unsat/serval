
include riscv-tests/isa/rv64ui/Makefrag
include riscv-tests/isa/rv64um/Makefrag
include riscv-tests/isa/rv32ui/Makefrag

RISCV_TESTS        := $(addsuffix .asm.rkt, $(addprefix $(O)/riscv-tests/isa/rv64ui/, $(rv64ui_sc_tests)))
RISCV_TESTS        += $(addsuffix .asm.rkt, $(addprefix $(O)/riscv-tests/isa/rv32ui/, $(rv32ui_sc_tests)))
RISCV_TESTS        += $(addsuffix .asm.rkt, $(addprefix $(O)/riscv-tests/isa/rv64um/, $(rv64um_sc_tests)))

RISCV_TEST_MARCH=-march=rv64im -mabi=lp64

$(O)/riscv-tests/%.elf: riscv-tests/%.S
	$(Q)$(MKDIR_P) $(@D)
	$(QUIET_CC)$(CC) "$^" -o "$@" \
	    -nostartfiles -nostdlib -mcmodel=medany -static $(RISCV_TEST_MARCH) \
		-Wl,-Ttext=0x80000000 \
		-I include \
		-I riscv-tests \
		-I riscv-tests/isa/macros/scalar

$(O)/riscv-tests/isa/rv32ui/%.elf: RISCV_TEST_MARCH=-march=rv32im -mabi=ilp32

check-riscv-tests: $(RISCV_TESTS)
	$(RACO_TEST) ++args "$^" -- riscv-tests/riscv-tests.rkt

.PHONY: check-riscv-tests