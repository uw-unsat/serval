#lang racket

(require "bpftests.rkt")

(define jmp32-tests (test-suite+ "Tests for BPF (jmp32)"
  (bpf-test-case "JMP32: JLT X"
    #:result #x2
    (BPF_ALU64_IMM BPF_MOV R0 0)
    (BPF_ALU64_IMM BPF_MOV R2 2)
    (BPF_ALU64_IMM BPF_LSH R2 40)
    (BPF_JMP32_REG BPF_JEQ R0 R2 1)
    (BPF_EXIT_INSN)
    (BPF_ALU64_IMM BPF_MOV R0 2)
    (BPF_EXIT_INSN))

  (bpf-test-case "JMP32: JLT K"
    #:result #x2
    (BPF_ALU64_IMM BPF_MOV R0 0)
    (BPF_ALU64_IMM BPF_MOV R1 2)
    (BPF_ALU64_IMM BPF_LSH R1 40)
    (BPF_JMP32_IMM BPF_JEQ R1 0 1)
    (BPF_EXIT_INSN)
    (BPF_ALU64_IMM BPF_MOV R0 2)
    (BPF_EXIT_INSN))))

(module+ test
  (time (run-tests jmp32-tests)))
