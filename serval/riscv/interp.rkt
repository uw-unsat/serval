#lang rosette

(require
  (prefix-in core: "../lib/core.rkt")
  (only-in racket/base hash-has-key? hash-ref)
  "base.rkt")

(provide (all-defined-out) (all-from-out "base.rkt"))

; Instruction is an opcode, destination register, two source registers, an immediate, and the size
; (in bytes) of the encoded form of the instruction.
; All except opcode and size are optional, and can be replaced with #f if not used for that particular
; instruction.
(struct instr (op dst src1 src2 imm size) #:transparent)

(struct program (base instructions) #:transparent)

(define (cpu-next! cpu size)
  (set-cpu-pc! cpu (bvadd (bv size (XLEN)) (cpu-pc cpu))))

; memory

(struct ptr (block path) #:transparent)

(define (memop->size op)
  (case op
    [(lb lbu sb) 1]
    [(lh lhu sh) 2]
    [(lw lwu sw) 4]
    [(ld ldu sd) 8]
    [else (core:bug-on #t #:dbg current-pc-debug #:msg (format "No such memop ~e\n" op))]))

(define (load-signed? op)
  (case op
    [(lb lh lw ld) #t]
    [(lbu lhu lwu ldu) #f]
    [else (core:bug-on #t #:dbg current-pc-debug #:msg (format "No such load ~e\n" op))]))

(define (resolve-mem-path cpu instr)
  (define type (instr-op instr))
  (define off (instr-imm instr))
  (define reg (instr-src1 instr))

  (define mr (core:guess-mregion-from-addr #:dbg current-pc-debug (cpu-mregions cpu) (gpr-ref cpu reg) off))
  (define start (core:mregion-start mr))
  (define end (core:mregion-end mr))
  (define name (core:mregion-name mr))
  (define block (core:mregion-block mr))

  (define addr (bvadd (sign-extend off (bitvector (XLEN))) (gpr-ref cpu reg)))
  (define size (core:bvpointer (memop->size type)))
  (define offset (bvsub addr (bv start (XLEN))))

  (core:bug-on (! (core:mregion-inbounds? mr addr size))
                #:dbg current-pc-debug
                #:msg (format "resolve-mem-path: address out of range:\n addr: ~e\n block: ~e" addr name))
  (define path (core:mblock-path block offset size #:dbg current-pc-debug))
  (ptr block path))

; conditionals

(define (evaluate-binary-conditional type val1 val2)
  (case type
    [(bge) (bvsge val1 val2)]
    [(blt) (bvslt val1 val2)]
    [(bgeu) (bvuge val1 val2)]
    [(bltu) (bvult val1 val2)]
    [(bne) (! (bveq val1 val2))]
    [(beq) (bveq val1 val2)]
    [else (core:bug-on #t #:dbg current-pc-debug #:msg (format "No such binary conditional ~e\n" type))]))

; ALU

(define (evaluate-binary-op type v1 v2)
  (case type
    [(addi addw add) (bvadd v1 v2)]
    [(subi subw sub) (bvsub v1 v2)]
    [(ori or) (bvor v1 v2)]
    [(andi and) (bvand v1 v2)]
    [(xori xor) (bvxor v1 v2)]
    [(slliw slli sllw sll) (bvshl v1 (bvand (bv (sub1 (core:bv-size v1)) (core:bv-size v1)) v2))]
    [(srliw srli srlw srl) (bvlshr v1 (bvand (bv (sub1 (core:bv-size v1)) (core:bv-size v1)) v2))]
    [(sraiw srai sraw sra) (bvashr v1 (bvand (bv (sub1 (core:bv-size v1)) (core:bv-size v1)) v2))]
    [(muli mulw mul) (bvmul v1 v2)]
    ; our code doesn't really use divisions - just add for completeness
    ; smtlib seems to have a different div-by-zero semantics for bvsdiv
    ; (bvsdiv -1 0) returns 1, while riscv returns -1
    [(divw div) (if (core:bvzero? v2) (bv -1 (core:bv-size v1)) (bvsdiv v1 v2))]
    [(remw rem) (if (core:bvzero? v2) v1 (bvsrem v1 v2))]
    [(divuw divu) (bvudiv v1 v2)]
    [(remuw remu) (bvurem v1 v2)]
    [else (core:bug-on #t #:dbg current-pc-debug #:msg (format "No such binary op ~e\n" type))]))

; jumps

(define (jump-and-link cpu reg addr #:size size)

  (define target
    (bvadd
      (cpu-pc cpu)
      (bvshl
        (sign-extend addr (bitvector (XLEN)))
        (bv 1 (XLEN)))))

  ; Set register to address of following instruction
  (gpr-set! cpu reg (bvadd (bv size (XLEN)) (cpu-pc cpu)))
  (set-cpu-pc! cpu target))

(define (do-csr-op cpu op dst csr value)
  (when (! (= 0 (gpr->idx dst)))
    (gpr-set! cpu dst (zero-extend (csr-ref cpu csr) (bitvector (XLEN)))))
  (case op
    [(csrrw csrrwi)
      (csr-set! cpu csr value)]
    [(csrrs csrrsi)
      (csr-set! cpu csr (bvor (csr-ref cpu csr) value))]
    [(csrrc csrrci)
      (csr-set! cpu csr (bvand (csr-ref cpu csr) (bvnot value)))]))

(define (check-imm-size size imm)
  (core:bug-on (! (= size (for/all ([imm imm #:exhaustive]) (core:bv-size imm))))
               #:msg (format "Bad immediate size: Expected ~e got ~e" (bitvector size) imm)
               #:dbg current-pc-debug))

; interpret one instr
(define (interpret-instr cpu instr)
  (define op (instr-op instr))
  (define dst (instr-dst instr))
  (define src1 (instr-src1 instr))
  (define src (instr-src1 instr))
  (define src2 (instr-src2 instr))
  (define imm (instr-imm instr))
  (define off (instr-imm instr))
  (define addr (instr-imm instr))
  (define size (instr-size instr))
  (case op
    [(nop wfi sfence.vma fence.i fence ecall ebreak)
      (cpu-next! cpu size)]

    ; AUIPC appends 12 low-order zero bits to the 20-bit U-immediate, sign-extends
    ; the result to 64 bits, then adds it to the pc and places the result in register rd.
    [(auipc)
      (check-imm-size 20 imm)
      (gpr-set! cpu dst (bvadd
                        (sign-extend (concat off (bv 0 12)) (bitvector (XLEN)))
                        (cpu-pc cpu)))
      (cpu-next! cpu size)]

    ; LUI places the 20-bit U-immediate into bits 31–12 of register rd and places
    ; zero in the lowest 12 bits. The 32-bit result is sign-extended to 64 bits.
    [(lui)
      (check-imm-size 20 imm)
      (gpr-set! cpu dst (sign-extend (concat imm (bv 0 12)) (bitvector (XLEN))))
      (cpu-next! cpu size)]

    [(sltiu)
      (check-imm-size 12 imm)
      (gpr-set! cpu dst
        (if (bvult (gpr-ref cpu src) (sign-extend imm (bitvector (XLEN))))
          (bv 1 (XLEN))
          (bv 0 (XLEN))))
      (cpu-next! cpu size)]

    [(sltu)
      (gpr-set! cpu dst
        (if (bvult (gpr-ref cpu src1) (gpr-ref cpu src2))
          (bv 1 (XLEN))
          (bv 0 (XLEN))))
      (cpu-next! cpu size)]

    ; ADDIW is an RV64I-only instruction that adds the sign-extended 12-bit
    ; immediate to register rs1 and produces the proper sign-extension of a 32-bit
    ; result in rd. Overflows are ignored and the result is the low 32 bits of the
    ; result sign-extended to 64 bits. Note, ADDIW rd, rs1, 0 writes the
    ; sign-extension of the lower 32 bits of register rs1 into register rd
    [(addiw)
      (check-imm-size 12 imm)
      (core:bug-on (! (= (XLEN) 64)) #:msg "addiw: (XLEN) != 64" #:dbg current-pc-debug)
      (gpr-set! cpu dst
        (sign-extend
          (bvadd (extract 31 0 (gpr-ref cpu src))
                 (sign-extend imm (bitvector 32)))
        (bitvector 64)))
      (cpu-next! cpu size)]

    [(slliw srliw sraiw)
      (check-imm-size 12 imm)
      (core:bug-on (! (= (XLEN) 64)) #:msg "slliw/srliw/sraiw: (XLEN) != 64" #:dbg current-pc-debug)
      (gpr-set! cpu dst
        (sign-extend
          (evaluate-binary-op op
            (extract 31 0 (gpr-ref cpu src))
            (bvand (sign-extend imm (bitvector 32)) (bv #b11111 32)))
          (bitvector 64)))
      (cpu-next! cpu size)]


    ; The jump and link (JAL) instruction uses the J-type format, where the J-immediate encodes a
    ; signed offset in multiples of 2 bytes. The offset is sign-extended and added to the address of
    ; the jump instruction to form the jump target address.
    [(jal)
      (check-imm-size 20 imm)
      (jump-and-link cpu dst addr #:size size)]

    ; The indirect jump instruction JALR (jump and link register) uses the I-type encoding.
    ; The target address is obtained by adding the sign-extended 12-bit I-immediate to the register
    ; rs1, then setting the least-significant bit of the result to zero. The address of the
    ; instruction following the jump (pc+4) is written to register rd.
    [(jalr)
      (check-imm-size 12 off)
      (gpr-set! cpu dst (bvadd (bv size (XLEN)) (cpu-pc cpu)))
      (define target
        (for/all ([src (gpr-ref cpu src) #:exhaustive])
          (bvand
            (bvnot (bv 1 (XLEN)))
            (bvadd src (sign-extend off (bitvector (XLEN)))))))
      (set-cpu-pc! cpu target)]

    [(ld ldu lw lwu lh lhu lb lbu)
      (check-imm-size 12 imm)
      (define ptr (resolve-mem-path cpu instr))
      (define block (ptr-block ptr))
      (define path (ptr-path ptr))
      (define extend (if (load-signed? op) sign-extend zero-extend))
      (gpr-set! cpu dst (extend (core:mblock-iload block path) (bitvector (XLEN))))
      (cpu-next! cpu size)]

    [(sd sw sh sb)
      (check-imm-size 12 imm)
      (define ptr (resolve-mem-path cpu instr))
      (define block (ptr-block ptr))
      (define path (ptr-path ptr))
      (define value (extract (- (* 8 (memop->size op)) 1) 0 (gpr-ref cpu src2)))
      (core:mblock-istore! block value path)
      (cpu-next! cpu size)]

    ; CSR reg
    [(csrrw csrrs csrrc)
      (do-csr-op cpu op dst src1 (gpr-ref cpu src2))
      (cpu-next! cpu size)]

    ; CSR imm
    [(csrrwi csrrsi csrrci)
      (check-imm-size 5 imm)
      (do-csr-op cpu op dst src1 (zero-extend imm (bitvector (XLEN))))
      (cpu-next! cpu size)]

    ; ; Binary conditional branch
    [(bge blt bgeu bltu bne beq)
      (check-imm-size 12 addr)

      (define (simplify-condition expr)
        (match expr
          [(expression (== !)
              (expression (== bveq)
                (expression (== bvadd) C1 x)
                (expression (== bvadd) C2 y)))
          #:when (&& (! (term? C1))
                      (! (term? C2))
                      (! (bveq C1 C2)))
          (define newexpr (|| (bveq x y) (! (bveq (bvadd C1 x) (bvadd C2 y)))))
          (core:bug-on (! (equal? newexpr expr)))
          newexpr]
          [_ expr]))

      (if (simplify-condition (evaluate-binary-conditional op (gpr-ref cpu src1) (gpr-ref cpu src2)))
        (jump-and-link cpu 'x0 addr #:size size)
        (cpu-next! cpu size))]

    ; Binary operation with immediate
    [(addi subi muli ori andi xori srli srai slli)
      (check-imm-size 12 imm)
      (gpr-set! cpu dst (evaluate-binary-op op (gpr-ref cpu src) (sign-extend imm (bitvector (XLEN)))))
      (cpu-next! cpu size)]

    ; Binary operation two registers
    [(add sub or and xor srl sra sll mul div rem divu remu)
      (gpr-set! cpu dst (evaluate-binary-op op (gpr-ref cpu src1) (gpr-ref cpu src2)))
      (cpu-next! cpu size)]

    ; Wide multiplication operations
    [(mulh mulhu mulhsu)
      (define a
        ((case op
          [(mulh mulhsu) sign-extend]
          [(mulhu) zero-extend])
        (gpr-ref cpu src1)
        (bitvector (* 2 (XLEN)))))
      (define b
        ((case op
          [(mulh) sign-extend]
          [(mulhu mulhsu) zero-extend])
        (gpr-ref cpu src2)
        (bitvector (* 2 (XLEN)))))
      (gpr-set! cpu dst
        (extract (- (* 2 (XLEN)) 1) (XLEN) (bvmul a b)))
      (cpu-next! cpu size)]

    ; Binary operation two registers (32-bit ops on 64-bit only)
    [(addw subw sllw srlw sraw mulw divw remw divuw remuw)
      (core:bug-on (! (= (XLEN) 64)) #:msg "*w: (XLEN) != 64" #:dbg current-pc-debug)
      (gpr-set! cpu dst (sign-extend (evaluate-binary-op op (extract 31 0 (gpr-ref cpu src1)) (extract 31 0 (gpr-ref cpu src2))) (bitvector 64)))
      (cpu-next! cpu size)]

    [else (core:bug-on #t #:dbg current-pc-debug #:msg (format "No semantics for instr ~e\n" instr))]))

(define (interpret-program cpu program)
  (define instructions (program-instructions program))
  (core:split-pc (cpu pc) cpu
    (define pc (cpu-pc cpu))
    (set-current-pc-debug! pc)
    (cond
      [(hash-has-key? (cpu-shims cpu) pc)
        ((hash-ref (cpu-shims cpu) pc) cpu)
        (interpret-program cpu program)]

      [(hash-has-key? instructions pc)
        (case (instr-op (hash-ref instructions pc))
          [(mret) (void)]
          [else
            (interpret-instr cpu (hash-ref instructions pc))
            (interpret-program cpu program)])])))