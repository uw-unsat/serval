
include riscv-tests/isa/rv64ui/Makefrag
include riscv-tests/isa/rv64um/Makefrag
include riscv-tests/isa/rv64ua/Makefrag
include riscv-tests/isa/rv64uc/Makefrag
include riscv-tests/isa/rv32ui/Makefrag
include riscv-tests/isa/rv32um/Makefrag
include riscv-tests/isa/rv32ua/Makefrag
include riscv-tests/isa/rv32uc/Makefrag

RISCV_TESTS        := $(addsuffix .asm.rkt, $(addprefix $(O)/riscv-tests/isa/rv64ui/, $(rv64ui_sc_tests)))
RISCV_TESTS        += $(addsuffix .asm.rkt, $(addprefix $(O)/riscv-tests/isa/rv64um/, $(rv64um_sc_tests)))
RISCV_TESTS        += $(addsuffix .asm.rkt, $(addprefix $(O)/riscv-tests/isa/rv64ua/, $(rv64ua_sc_tests)))
# RISCV_TESTS        += $(addsuffix .asm.rkt, $(addprefix $(O)/riscv-tests/isa/rv64uc/, $(rv64uc_sc_tests)))
RISCV_TESTS        += $(addsuffix .asm.rkt, $(addprefix $(O)/riscv-tests/isa/rv32ui/, $(rv32ui_sc_tests)))
RISCV_TESTS        += $(addsuffix .asm.rkt, $(addprefix $(O)/riscv-tests/isa/rv32um/, $(rv32um_sc_tests)))
RISCV_TESTS        += $(addsuffix .asm.rkt, $(addprefix $(O)/riscv-tests/isa/rv32ua/, $(rv32ua_sc_tests)))
# RISCV_TESTS        += $(addsuffix .asm.rkt, $(addprefix $(O)/riscv-tests/isa/rv32uc/, $(rv32uc_sc_tests)))

RISCV_MAPS         := $(patsubst %.asm.rkt,%.map.rkt,$(RISCV_TESTS))

RISCV_TEST_MARCH=-march=rv64g -mabi=lp64

$(O)/riscv-tests/%.elf: riscv-tests/%.S
	$(Q)$(MKDIR_P) $(@D)
	$(QUIET_CC)$(CC) "$^" -o "$@" \
	    -fno-pic -fno-pie -fno-plt -nostartfiles -nostdlib -mcmodel=medany -static $(RISCV_TEST_MARCH) \
		-Wl,-Ttext=0x80000000 \
		-I include \
		-I riscv-tests \
		-I riscv-tests/isa/macros/scalar

$(O)/riscv-tests/isa/rv32ui/%.elf: RISCV_TEST_MARCH=-march=rv32g -mabi=ilp32
$(O)/riscv-tests/isa/rv32um/%.elf: RISCV_TEST_MARCH=-march=rv32g -mabi=ilp32
$(O)/riscv-tests/isa/rv32ua/%.elf: RISCV_TEST_MARCH=-march=rv32g -mabi=ilp32

check-riscv-tests: \
		$(RISCV_TESTS) \
		$(patsubst %.asm.rkt,%.map.rkt,$(RISCV_TESTS)) \
		$(patsubst %.asm.rkt,%.globals.rkt,$(RISCV_TESTS))
	$(RACO_TEST) ++args "$(RISCV_TESTS)" -- riscv-tests/riscv-tests.rkt

.PHONY: check-riscv-tests