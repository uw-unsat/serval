#!/bin/sh

./const_generator.py python/unicorn_const.py \
	--enum UC_ARCH \
	--enum UC_ERR \
	--enum UC_MEM \
	--enum UC_HOOK \
	--bitmask UC_MODE \
	--bitmask UC_PROT \
	> const/unicorn.rkt

./const_generator.py python/x86_const.py \
	--enum UC_X86_REG \
	--enum UC_X86_INS_ \
	> const/x86.rkt
