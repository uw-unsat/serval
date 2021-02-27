#lang rosette

(require
  (prefix-in core: "lib/core.rkt")
  rosette/base/core/polymorphic)

(provide (all-defined-out))

(define bpf-strict-pointer (make-parameter #f))

(define current-pc-debug #f)

(struct insn (code dst src off imm) #:transparent)

; Callmgr decides how to handle BPF_CALL / BPF_TAIL_CALL insns
(define-generics callmgr
  (callmgr-handle-call callmgr bpf-cpu bpf-insn))

(define (fd->map cpu fd)
  (vector-ref (cpu-fdtable cpu) (bitvector->natural fd)))

(define (evaluate-call-map_lookup_elem cpu)
  (define fd (reg-ref cpu BPF_REG_ARG1))
  (define keyp (reg-ref cpu BPF_REG_ARG2))
  (bpf-map-lookup (fd->map cpu fd) keyp))

(define bpf-calls (vector #f
  evaluate-call-map_lookup_elem))

; Default callmgr dispatches to a vector of (Racket) functions using immediate in instruction.
(struct default-callmgr (calltable) #:transparent
  #:methods gen:callmgr
  [(define (callmgr-handle-call callmgr cpu insn)
    (define code (insn-code insn))
    (core:bug-on (equal? code '(BPF_JMP BPF_TAIL_CALL)) #:msg "default-callmgr: no tail calls")
    (define imm (insn-imm insn))
    (define f (vector-ref (default-callmgr-calltable callmgr) (bitvector->natural imm)))
    (reg-set! cpu BPF_REG_0 (f cpu))
    (for-each (lambda (r) (reg-havoc! cpu r))
              (list BPF_REG_1 BPF_REG_2 BPF_REG_3 BPF_REG_4 BPF_REG_5)))])

(define (make-default-callmgr [calltable bpf-calls])
  (default-callmgr calltable))

(struct cpu (pc regs fdtable tail-call-cnt memmgr callmgr) #:mutable #:transparent
  #:methods gen:custom-write
  [(define (write-proc cpu port mode)
     (define regs (cpu-regs cpu))
     (define fdtable (cpu-fdtable cpu))
     (fprintf port "(cpu")
     (fprintf port "\n  pc . ~a" (cpu-pc cpu))
     (fprintf port "\n  ~a" regs)
     (for ([i (in-range (vector-length fdtable))])
       (fprintf port "\n  fd~a . ~a" i (vector-ref fdtable i)))
     (fprintf port ")"))]
  #:methods core:gen:gen-cpu
  [(define (gen-cpu-memmgr cpu) (cpu-memmgr cpu))
   (define (gen-cpu-pc cpu) (cpu-pc cpu))])

(struct regs (r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 ax) #:transparent #:mutable)

; register numbers

(define BPF_REG_0  'r0)
(define BPF_REG_1  'r1)
(define BPF_REG_2  'r2)
(define BPF_REG_3  'r3)
(define BPF_REG_4  'r4)
(define BPF_REG_5  'r5)
(define BPF_REG_6  'r6)
(define BPF_REG_7  'r7)
(define BPF_REG_8  'r8)
(define BPF_REG_9  'r9)
(define BPF_REG_10 'r10)
(define BPF_REG_AX 'ax) ; Auxiliary register hidden from userspace

(define MAX_BPF_JIT_REG 12)

; ArgX, context and stack frame pointer register positions. Note,
; Arg1, Arg2, Arg3, etc are used as argument mappings of function
; calls in BPF_CALL instruction.

(define BPF_REG_ARG1    BPF_REG_1)
(define BPF_REG_ARG2    BPF_REG_2)
(define BPF_REG_ARG3    BPF_REG_3)
(define BPF_REG_ARG4    BPF_REG_4)
(define BPF_REG_ARG5    BPF_REG_5)
(define BPF_REG_CTX     BPF_REG_6)
(define BPF_REG_FP      BPF_REG_10)

; Additional register mappings for converted user programs.
(define BPF_REG_A       BPF_REG_0)
(define BPF_REG_X       BPF_REG_7)
(define BPF_REG_TMP     BPF_REG_8)

; BPF program can access up to 512 bytes of stack space.

(define MAX_BPF_STACK   512)

(define BPF_PSEUDO_MAP_FD 1)

;  helper functions

(define BPF_FUNC_unspec          0)
(define BPF_FUNC_map_lookup_elem 1)
(define BPF_FUNC_map_update_elem 2)
(define BPF_FUNC_map_delete_elem 3)

(define-generics bpf-map
  (bpf-map-key-size bpf-map)
  (bpf-map-value-size bpf-map)
  (bpf-map-num-entries bpf-map)
  (bpf-map-lookup bpf-map key-ptr))

; ALU ops on registers, bpf_add|sub|...: dst_reg += src_reg

(define-syntax-rule (BPF_ALU64_REG OP DST SRC)
  (make-insn '(BPF_ALU64 OP BPF_X) DST SRC #f #f))

(define-syntax-rule (BPF_ALU32_REG OP DST SRC)
  (make-insn '(BPF_ALU OP BPF_X) DST SRC #f #f))

; ALU ops on immediates, bpf_add|sub|...: dst_reg += imm32

(define-syntax-rule (BPF_ALU64_IMM OP DST IMM)
  (case 'OP
    [(BPF_NEG) (make-insn '(BPF_ALU64 OP) DST #f #f #f)]
    [else (make-insn '(BPF_ALU64 OP BPF_K) DST #f #f IMM)]))

(define-syntax-rule (BPF_ALU32_IMM OP DST IMM)
  (case 'OP
    [(BPF_NEG) (make-insn '(BPF_ALU OP) DST #f #f #f)]
    [else (make-insn '(BPF_ALU OP BPF_K) DST #f #f IMM)]))

; Endianess conversion, cpu_to_{l,b}e(), {l,b}e_to_cpu()

(define-syntax-rule (BPF_ENDIAN TYPE DST LEN)
  (make-insn '(BPF_ALU BPF_END TYPE) DST #f #f LEN))

; Short form of mov, dst_reg = src_reg

(define (BPF_MOV64_REG DST SRC)
  (make-insn '(BPF_ALU64 BPF_MOV BPF_X) DST SRC #f #f))

(define (BPF_MOV32_REG DST SRC)
  (make-insn '(BPF_ALU BPF_MOV BPF_X) DST SRC #f #f))

; Short form of mov, dst_reg = imm32

(define (BPF_MOV64_IMM DST IMM)
  (make-insn '(BPF_ALU64 BPF_MOV BPF_K) DST #f #f IMM))

(define (BPF_MOV32_IMM DST IMM)
  (make-insn '(BPF_ALU BPF_MOV BPF_K) DST #f #f IMM))

; BPF_LD_IMM64 macro encodes single 'load 64-bit immediate' insn

(define (BPF_LD_IMM64 DST IMM)
  (BPF_LD_IMM64_RAW DST #f IMM))

(define (BPF_LD_IMM64_RAW DST SRC IMM)
  (list (make-insn '(BPF_LD BPF_IMM BPF_DW) DST SRC #f (bitwise-and IMM #xffffffff))
        (make-insn #f #f #f #f (arithmetic-shift IMM -32))))

; pseudo BPF_LD_IMM64 insn used to refer to process-local map_fd
(define (BPF_LD_MAP_FD DST MAP_FD)
  (BPF_LD_IMM64_RAW DST BPF_PSEUDO_MAP_FD MAP_FD))

; Memory load, dst_reg = *(uint *) (src_reg + off16)

(define-syntax-rule (BPF_LDX_MEM SIZE DST SRC OFF)
  (make-insn '(BPF_LDX BPF_MEM SIZE) DST SRC OFF #f))

; Memory store, *(uint *) (dst_reg + off16) = src_reg

(define-syntax-rule (BPF_STX_MEM SIZE DST SRC OFF)
  (make-insn '(BPF_STX BPF_MEM SIZE) DST SRC OFF #f))

; Atomic memory add, *(uint *)(dst_reg + off16) += src_reg

(define-syntax-rule (BPF_STX_XADD SIZE DST SRC OFF)
  (make-insn '(BPF_STX BPF_XADD SIZE) DST SRC OFF #f))

; Memory store, *(uint *) (dst_reg + off16) = imm32

(define-syntax-rule (BPF_ST_MEM SIZE DST OFF IMM)
  (make-insn '(BPF_ST BPF_MEM SIZE) DST #f OFF IMM))

; Conditional jumps against registers, if (dst_reg 'op' src_reg) goto pc + off16

(define-syntax-rule (BPF_JMP_REG OP DST SRC OFF)
  (make-insn '(BPF_JMP OP BPF_X) DST SRC OFF #f))

(define-syntax-rule (BPF_JMP32_REG OP DST SRC OFF)
  (make-insn '(BPF_JMP32 OP BPF_X) DST SRC OFF #f))

; Conditional jumps against immediates, if (dst_reg 'op' imm32) goto pc + off16

(define-syntax-rule (BPF_JMP_IMM OP DST IMM OFF)
  (make-insn '(BPF_JMP OP BPF_K) DST #f OFF IMM))

(define-syntax-rule (BPF_JMP32_IMM OP DST IMM OFF)
  (make-insn '(BPF_JMP32 OP BPF_K) DST #f OFF IMM))

; Function call

(define (BPF_EMIT_CALL FUNC)
  (make-insn '(BPF_JMP BPF_CALL) #f #f #f FUNC))

; Program exit

(define (BPF_EXIT_INSN)
  (make-insn '(BPF_JMP BPF_EXIT) #f #f #f #f))

; Debugging

(define (BPF_TRACE LOC)
  (make-insn '(BPF_TRACE) #f #f #f LOC))

(define (make-insn code dst src off imm)
  (insn code dst src
        (if (boolean? off) off (bv off 16))
        (if (boolean? imm) imm (bv imm 32))))

(define make-insns
  (lambda lst
    (define insns (flatten lst))
    (define keys (build-list (length insns) (lambda (x) (bv x 64))))
    (make-immutable-hash (map cons keys insns))))

(define (regs->vector regs)
  (vector (regs-r0 regs) (regs-r1 regs) (regs-r2 regs) (regs-r3 regs)
          (regs-r4 regs) (regs-r5 regs) (regs-r6 regs) (regs-r7 regs)
          (regs-r8 regs) (regs-r9 regs) (regs-r10 regs) (regs-ax regs)))

(define (reg->idx reg)
  (case reg
    [(r0) 0]
    [(r1) 1]
    [(r2) 2]
    [(r3) 3]
    [(r4) 4]
    [(r5) 5]
    [(r6) 6]
    [(r7) 7]
    [(r8) 8]
    [(r9) 9]
    [(r10 fp) 10]
    [(ax) 11]
    [else (core:bug #:msg (format "reg->idx: Unknown reg ~v" reg))]))

(define (idx->reg idx)
  (case idx
    [(0) 'r0]
    [(1) 'r1]
    [(2) 'r2]
    [(3) 'r3]
    [(4) 'r4]
    [(5) 'r5]
    [(6) 'r6]
    [(7) 'r7]
    [(8) 'r8]
    [(9) 'r9]
    [(10) 'r10]
    [(11) 'ax]
    [else (core:bug #:msg (format "idx->reg: Unknown idx ~v" idx))]))

(define (make-regs [value #f])
  (apply regs (make-list MAX_BPF_JIT_REG value)))

(define (init-cpu [ctx #f]
                  [fdtable (vector)]
                  #:make-memmgr [make-memmgr core:make-flat-memmgr]
                  #:make-callmgr [make-callmgr make-default-callmgr])

  ; initially regs are uninitialized and must be written before read
  (define regs (make-regs))
  ; R1 points to context
  (set-regs-r1! regs ctx)
  ; R10 points to the stack, which is uninitialized
  (define-symbolic* stack (bitvector 64))
  (set-regs-r10! regs stack)
  (define-symbolic* tail-call-cnt (bitvector 32))
  (cpu (bv 0 64) regs fdtable tail-call-cnt (make-memmgr) (make-callmgr)))

(define (@reg-set! regs reg val)
  (case reg
    [(r0) (set-regs-r0! regs val)]
    [(r1) (set-regs-r1! regs val)]
    [(r2) (set-regs-r2! regs val)]
    [(r3) (set-regs-r3! regs val)]
    [(r4) (set-regs-r4! regs val)]
    [(r5) (set-regs-r5! regs val)]
    [(r6) (set-regs-r6! regs val)]
    [(r7) (set-regs-r7! regs val)]
    [(r8) (set-regs-r8! regs val)]
    [(r9) (set-regs-r9! regs val)]
    [(r10 fp) (core:bug #:msg "reg-set!: R10/FP is read-only")]
    [(ax) (set-regs-ax! regs val)]
    [else (core:bug #:msg (format "reg-set!: Unknown register ~v" reg))]))

(define (reg-set! cpu reg val)
  (@reg-set! (cpu-regs cpu) reg val))

(define (reg-havoc! cpu reg)
  (core:bug-on (equal? reg BPF_REG_10)
               #:msg (format "reg-havoc!: R10 is read-only"))
  (define-symbolic* havoc (bitvector 64))
  (reg-set! cpu reg havoc))

(define (@reg-ref regs reg)
  (define val
    (case reg
      [(r0) (regs-r0 regs)]
      [(r1) (regs-r1 regs)]
      [(r2) (regs-r2 regs)]
      [(r3) (regs-r3 regs)]
      [(r4) (regs-r4 regs)]
      [(r5) (regs-r5 regs)]
      [(r6) (regs-r6 regs)]
      [(r7) (regs-r7 regs)]
      [(r8) (regs-r8 regs)]
      [(r9) (regs-r9 regs)]
      [(r10 fp) (regs-r10 regs)]
      [(ax) (regs-ax regs)]
      [else (core:bug #:msg (lambda (sol) (format "reg-ref: unknown reg ~v" (evaluate reg sol))))]))
  val)

(define (reg-ref cpu reg)
  (@reg-ref (cpu-regs cpu) reg))

(define (evaluate-alu64 op v1 v2)
  (case op
    [(BPF_ADD) (bvadd v1 v2)]
    [(BPF_SUB) (bvsub v1 v2)]
    [(BPF_MUL) ((core:bvmul-proc) v1 v2)]
    [(BPF_DIV)
     (core:bug-on (bvzero? v2)
                  #:msg (format "evaluate-alu64: div by zero, dividend: ~v" v2))
     ((core:bvudiv-proc) v1 v2)]
    [(BPF_MOD)
     (core:bug-on (bvzero? v2)
                  #:msg (format "evaluate-alu64: mod by zero, dividend: ~v" v2))
     ((core:bvurem-proc) v1 v2)]
    [(BPF_OR) (bvor v1 v2)]
    [(BPF_AND) (bvand v1 v2)]
    [(BPF_XOR) (bvxor v1 v2)]
    [(BPF_LSH) (bvshl v1 v2)]
    [(BPF_RSH) (bvlshr v1 v2)]
    [(BPF_ARSH) (bvashr v1 v2)]
    [else (core:bug #:msg (format "evaluate-alu64: no such ALU op: ~e\n" op))]))

(define (evaluate-alu32 op v1 v2)
  (zero-extend (evaluate-alu64 op (extract 31 0 v1) (extract 31 0 v2)) (bitvector 64)))

(define (evaluate-cond op v1 v2)
  (case op
    [(BPF_JEQ) (bveq v1 v2)]
    [(BPF_JNE) (! (evaluate-cond 'BPF_JEQ v1 v2))]
    ; maybe too strict for pointer comparisons
    [(BPF_JGT) (bvugt v1 v2)]
    [(BPF_JLT) (bvult v1 v2)]
    [(BPF_JGE) (bvuge v1 v2)]
    [(BPF_JLE) (bvule v1 v2)]
    [(BPF_JSET) (! (bvzero? (bvand v1 v2)))]
    [(BPF_JSGT) (bvsgt v1 v2)]
    [(BPF_JSLT) (bvslt v1 v2)]
    [(BPF_JSGE) (bvsge v1 v2)]
    [(BPF_JSLE) (bvsle v1 v2)]
    [else (core:bug #:msg (format "no such comparison op: ~e\n" op))]))

(define (bpf-size->integer size)
  (case size
    [(BPF_B) 1]
    [(BPF_H) 2]
    [(BPF_W) 4]
    [(BPF_DW) 8]
    [else (core:bug #:msg (format "unknown BPF_SIZE: ~e\n" size))]))

; assume little-endian
(define bitvector->list core:bitvector->list/le)
(define list->bitvector core:list->bitvector/le)

(define (store-bytes! cpu addr off data sizen)
  (define memmgr (cpu-memmgr cpu))
  ; Truncate addr to underlying memory model bitwidth.
  (set! addr (extract (- (core:memmgr-bitwidth memmgr) 1) 0 addr))
  (set! data (extract (- (* 8 sizen) 1) 0 data))
  (set! off (sign-extend off (type-of addr)))
  (core:memmgr-store! memmgr addr off data (integer->bitvector sizen (type-of addr))))

(define (load-bytes cpu addr off sizen)
  (define memmgr (cpu-memmgr cpu))
  ; Truncate addr to underlying memory model bitwidth.
  (set! addr (extract (- (core:memmgr-bitwidth memmgr) 1) 0 addr))
  (set! off (sign-extend off (type-of addr)))
  (define value (core:memmgr-load memmgr addr off (integer->bitvector sizen (type-of addr))))
  value)

(define (cpu-next! cpu [size (bv 1 64)])
  (set-cpu-pc! cpu (bvadd size (cpu-pc cpu))))

(define (sign-imm64 x)
  (sign-extend x (bitvector 64)))

(define (zero-imm32 x)
  (zero-extend x (bitvector 32)))

(define (zero-imm64 x)
  (zero-extend x (bitvector 64)))

(define (imm64-dw lo hi)
  (concat (zero-imm32 hi)
          (zero-imm32 lo)))

(define (endian-size imm)
  (cond
    [(equal? imm (bv 16 32)) 2]
    [(equal? imm (bv 32 32)) 4]
    [(equal? imm (bv 64 32)) 8]
    [else (core:bug #:msg (format "endian-size: unknown endian size ~e\n" imm))]))

; The static "size" of a BPF instruction. This is the amount the PC must be incremented by to point
; to the next instruction in the program. It is 1 for all instructions except ld64, which is unique.
(define (insn-size insn)
  (case (insn-code insn)
    [((BPF_LD BPF_IMM BPF_DW)) (bv 2 64)]
    [else (bv 1 64)]))

; Interpret an instruction.
(define (interpret-insn cpu insn #:next [next-insn #f])
  (define code (insn-code insn))
  (define dst (insn-dst insn))
  (define src (insn-src insn))
  (define off (insn-off insn))
  (define imm (insn-imm insn))
  (define size (insn-size insn))
  (define memmgr (cpu-memmgr cpu))
  (define callmgr (cpu-callmgr cpu))

  (match code
    ; 64-bit immediate load
    [(list 'BPF_LD 'BPF_IMM 'BPF_DW)
      ; Use this instruction for imm[31:0], next instruction for imm[63:32]
      (core:bug-on (! (insn? next-insn))
                   #:msg "interpret-insn: must pass next instruction for BPF_LD BPF_IMM BPF_DW")
      (define lower imm)
      (define upper (insn-imm next-insn))
      (reg-set! cpu dst (imm64-dw lower upper))]

    ; debugging
    [(list 'BPF_TRACE)
     (displayln (cons (bitvector->natural imm) cpu))]

    ; neg instructions (before other ALU operations)
    [(list 'BPF_ALU64 'BPF_NEG)
     (reg-set! cpu dst (bvneg (reg-ref cpu dst)))]

    [(list 'BPF_ALU 'BPF_NEG)
     (reg-set! cpu dst (zero-extend (bvneg (extract 31 0 (reg-ref cpu dst))) (bitvector 64)))]

    ; mov instructions (before other ALU operations)
    [(list 'BPF_ALU64 'BPF_MOV 'BPF_K)
     (reg-set! cpu dst (sign-imm64 imm))]

    [(list 'BPF_ALU 'BPF_MOV 'BPF_K)
     (reg-set! cpu dst (zero-imm64 imm))]

    [(list 'BPF_ALU64 'BPF_MOV 'BPF_X)
     (reg-set! cpu dst (reg-ref cpu src))]

    [(list 'BPF_ALU 'BPF_MOV 'BPF_X)
     (reg-set! cpu dst (zero-extend (extract 31 0 (reg-ref cpu src)) (bitvector 64)))]

    ; binary operation with immediate
    [(list 'BPF_ALU64 op 'BPF_K)
      (reg-set! cpu dst (evaluate-alu64 op (reg-ref cpu dst) (sign-imm64 imm)))]

    [(list 'BPF_ALU op 'BPF_K)
     (reg-set! cpu dst (evaluate-alu32 op (reg-ref cpu dst) imm))]

    ; binary operation two registers
    [(list 'BPF_ALU64 op 'BPF_X)
     (cond
       [(&& (|| (equal? op 'BPF_SUB) (equal? op 'BPF_XOR)) (equal? dst src))
        (reg-set! cpu dst (bv 0 64))]
       [else
        (reg-set! cpu dst (evaluate-alu64 op (reg-ref cpu dst) (reg-ref cpu src)))])]

    [(list 'BPF_ALU op 'BPF_X)
     (cond
       [(&& (|| (equal? op 'BPF_SUB) (equal? op 'BPF_XOR)) (equal? dst src))
        (reg-set! cpu dst (bv 0 64))]
       [else
        (reg-set! cpu dst (evaluate-alu32 op (reg-ref cpu dst) (reg-ref cpu src)))])]

    ; unconditional branching
    [(or (list 'BPF_JMP 'BPF_JA 'BPF_K) (list 'BPF_JMP 'BPF_JA))
     (cpu-next! cpu (sign-imm64 off))]

    ; conditional branching
    [(list 'BPF_JMP op 'BPF_K)
     (when (evaluate-cond op (reg-ref cpu dst) (sign-imm64 imm))
       (cpu-next! cpu (sign-imm64 off)))]

    [(list 'BPF_JMP op 'BPF_X)
     (when (evaluate-cond op (reg-ref cpu dst) (reg-ref cpu src))
       (cpu-next! cpu (sign-imm64 off)))]

    [(list 'BPF_JMP32 op 'BPF_K)
     (when (evaluate-cond op (extract 31 0 (reg-ref cpu dst)) imm)
       (cpu-next! cpu (sign-imm64 off)))]

    [(list 'BPF_JMP32 op 'BPF_X)
     (when (evaluate-cond op (extract 31 0 (reg-ref cpu dst)) (extract 31 0 (reg-ref cpu src)))
       (cpu-next! cpu (sign-imm64 off)))]

    ; call / tail call
    [(list 'BPF_JMP (or 'BPF_CALL 'BPF_TAIL_CALL))
      (callmgr-handle-call callmgr cpu insn)]

    [(list 'BPF_JMP 'BPF_EXIT)
      ; Mark end of execution by setting PC to #f.
      (set-cpu-pc! cpu #f)]

    ; load operations
    [(list 'BPF_LDX 'BPF_MEM size)
      (define addr (reg-ref cpu src))
      (define value (load-bytes cpu addr off (bpf-size->integer size)))
      (reg-set! cpu dst (zero-extend value (bitvector 64)))]

    ; store operations
    [(list 'BPF_ST 'BPF_MEM size)
      (define addr (reg-ref cpu dst))
      (store-bytes! cpu addr off (sign-imm64 imm) (bpf-size->integer size))]

    [(list 'BPF_STX 'BPF_MEM size)
      (define addr (reg-ref cpu dst))
      (define value (reg-ref cpu src))
      (store-bytes! cpu addr off value (bpf-size->integer size))]

    ; xadd operations
    [(list 'BPF_STX 'BPF_XADD 'BPF_W)
      (define addr (reg-ref cpu dst))
      (core:memmgr-atomic-begin memmgr)
      (define old (load-bytes cpu addr off 4))
      (define new (bvadd old (extract 31 0 (reg-ref cpu src))))
      (store-bytes! cpu addr off new 4)
      (core:memmgr-atomic-end memmgr)]

    [(list 'BPF_STX 'BPF_XADD 'BPF_DW)
      (define addr (reg-ref cpu dst))
      (core:memmgr-atomic-begin memmgr)
      (define old (load-bytes cpu addr off 8))
      (define new (bvadd old (reg-ref cpu src)))
      (store-bytes! cpu addr off new 8)
      (core:memmgr-atomic-end memmgr)]

    ; endian operations
    [(list 'BPF_ALU 'BPF_END 'BPF_FROM_BE)
      (define old (reg-ref cpu dst))
      (define new
        (cond
          [(equal? imm (bv 16 32))
            (zero-extend
              (concat (extract 7 0 old) (extract 15 8 old))
              (bitvector 64))]
          [(equal? imm (bv 32 32))
            (zero-extend
              (concat (extract 7 0 old) (extract 15 8 old) (extract 23 16 old) (extract 31 24 old))
              (bitvector 64))]
          [(equal? imm (bv 64 32))
            (zero-extend
              (concat (extract 7 0 old) (extract 15 8 old) (extract 23 16 old) (extract 31 24 old)
                      (extract 39 32 old) (extract 47 40 old) (extract 55 48 old) (extract 63 56 old))
              (bitvector 64))]
          [else (core:bug #:msg (format "BPF_ALU: imm must be one of {16,32,64}, got ~v" imm))]))
        (reg-set! cpu dst new)]

    [(list 'BPF_ALU 'BPF_END 'BPF_FROM_LE)
      (define old (reg-ref cpu dst))
      (define new
        (cond
          [(equal? imm (bv 16 32))
            (zero-extend (extract 15 0 old) (bitvector 64))]
          [(equal? imm (bv 32 32))
            (zero-extend (extract 31 0 old) (bitvector 64))]
          [(equal? imm (bv 64 32))
            old]
          [else (core:bug #:msg (format "BPF_ALU: imm must be one of {16,32,64}, got ~v" imm))]))
      (reg-set! cpu dst new)]

    ; default
    [_ (core:bug #:msg (format "interpret-insn: no semantics for instruction ~e\n" code))])

  ; size == (bv 1 64) for all instructions except ld64.
  (when (cpu-pc cpu)
    (cpu-next! cpu size)))

; Interpret a BPF program until BPF_JMP BPF_EXIT.
; cpu -> A BPF cpu struct
; instructions -> A hash from PC (bitvector 64) to struct insn.
(define (interpret-program cpu instructions)
  (for/all ([pc (cpu-pc cpu) #:exhaustive])
    (begin
      (set-cpu-pc! cpu pc)
      (cond
        ; A #f PC indicates that execution has terminated via BPF_EXIT.
        [(false? pc)
          (extract 31 0 (reg-ref cpu BPF_REG_0))]
        [(hash-has-key? instructions pc)
          (define this-insn (hash-ref instructions pc))
          (core:bug-on (! (insn? this-insn))
                       #:msg (format "interpret-program: need insn?, got ~v" this-insn))
          (case (insn-code this-insn)
            ; Handle ld64 specially because it requires fetching the next instruction.
            [((BPF_LD BPF_IMM BPF_DW))
              ; Fetch next instruction.
              (define pc2 (bvadd1 pc))
              (core:bug-on (! (hash-has-key? instructions pc2))
                          #:msg (format "no instruction after LD64 @ ~e\n" pc))
              (define next-insn (hash-ref instructions pc2))
              (interpret-insn cpu this-insn #:next next-insn)
              (interpret-program cpu instructions)]

            [else
              (interpret-insn cpu this-insn)
              (interpret-program cpu instructions)])]

        [else (core:bug #:msg (format "no instruction @ ~e\n" pc))]))))
