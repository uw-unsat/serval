check-bpf: bpf/test_bpf.rkt bpf/jmp32.rkt
	$(RACO_TEST) $^

bpf/test_bpf.rkt: bpf/test_bpf.c bpf/gen.py
	bpf/gen.py < $< > $@~
	mv $@~ $@
