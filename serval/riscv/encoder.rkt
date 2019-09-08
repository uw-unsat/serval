#lang rosette

(require "base.rkt" "interp.rkt")

(define (r-type funct7 rs2 rs1 funct3 rd opcode)
  (concat (bv funct7 7) rs2 rs1 (bv funct3 3) rd (bv opcode 7)))

(define (i-type imm rs1 funct3 rd opcode)
  (concat imm rs1 (bv funct3 3) rd (bv opcode 7)))

(define (s-type imm rs2 rs1 funct3 opcode)
  (concat (extract 11 5 imm) rs2 rs1 (bv funct3 3) (extract 4 0 imm) (bv opcode 7)))

(define (u-type imm rd opcode)
  (concat imm rd (bv opcode 7)))

(define (b-type imm rs2 rs1 funct3 opcode)
  (concat (extract 11 11 imm) (extract 9 4 imm) rs2 rs1
          (bv funct3 3) (extract 3 0 imm) (extract 10 10 imm) (bv opcode 7)))

(define (j-type imm rd opcode)
  (concat (extract 19 19 imm) (extract 9 0 imm) (extract 10 10 imm) (extract 18 11 imm)
          rd (bv opcode 7)))

(define (encode-gpr r)
  (bv (gpr->idx r) 5))

(define (encode-csr r)
  (define v
    (case r
      [(sstatus)    #x100]
      [(sedeleg)    #x102]
      [(sideleg)    #x103]
      [(sie)        #x104]
      [(stvec)      #x105]
      [(scounteren) #x106]

      [(sscratch)   #x140]
      [(sepc)       #x141]
      [(scause)     #x142]
      [(stval)      #x143]
      [(sip)        #x144]

      [(satp)       #x180]

      [(mvendorid)  #xF11]
      [(marchid)    #xF12]
      [(mimpid)     #xF13]
      [(mhartid)    #xF14]

      [(mstatus)    #x300]
      [(misa)       #x301]
      [(medeleg)    #x302]
      [(mideleg)    #x303]
      [(mie)        #x304]
      [(mtvec)      #x305]
      [(mcounteren) #x306]

      [(mscratch)   #x340]
      [(mepc)       #x341]
      [(mcause)     #x342]
      [(mtval)      #x343]
      [(mip)        #x344]

      [(pmpcfg0)    #x3A0]
      [(pmpcfg1)    #x3A1]
      [(pmpcfg2)    #x3A2]
      [(pmpcfg3)    #x3A3]

      [(pmpaddr0)   #x3B0]
      [(pmpaddr1)   #x3B1]
      [(pmpaddr2)   #x3B2]
      [(pmpaddr3)   #x3B3]
      [(pmpaddr4)   #x3B4]
      [(pmpaddr5)   #x3B5]
      [(pmpaddr6)   #x3B6]
      [(pmpaddr7)   #x3B7]
      [(pmpaddr8)   #x3B8]
      [(pmpaddr9)   #x3B9]
      [(pmpaddr10)  #x3BA]
      [(pmpaddr11)  #x3BB]
      [(pmpaddr12)  #x3BC]
      [(pmpaddr13)  #x3BD]
      [(pmpaddr14)  #x3BE]
      [(pmpaddr15)  #x3BF]

      [else         #f]))

  (cond
    [(equal? #f v)
      (printf "Warning: No CSR encoding for ~a\n" r)
      (bv 0 12)]
    [else (bv v 12)]))

(define (encode-instr insn)
  (define op (instr-op insn))
  (case op

    [(ecall)
      (i-type (bv #b000000000000 12) (bv 0 5) 0 (bv 0 5) #b1110011)]
    [(ebreak)
      (i-type (bv #b000000000001 12) (bv 0 5) 0 (bv 0 5) #b1110011)]
    [(mret)
      (r-type #b0011000 (bv #b00010 5) (bv 0 5) #b000 (bv 0 5) #b1110011)]
    [(wfi)
      (r-type #b0001000 (bv #b00101 5) (bv 0 5) #b000 (bv 0 5) #b1110011)]
    [(c.unimp)
      (bv 0 16)]
    [(unimp)
      (bv #xc0001073 32)]

    #|
      TODO: Handle these correctly.
      *fence* instructions take additional parameters as hints to hardware
      but the objdump parser doesn't know how to preserve these into our internal representation
      yet. For now, do best effort for sfence.vma and fence.i and ignore fence.
    |#
    [(sfence.vma)
      (r-type #b0001001 (bv 0 5) (bv 0 5) #b000 (bv 0 5) #b1110011)]
    [(fence)
      #f]
    [(fence.i)
      (i-type (bv 0 12) (bv 0 5) #b001 (bv 0 5) #b0001111)]

    [(csrrw csrrs csrrc csrrwi csrrsi csrrci)
      (define rd (encode-gpr (instr-dst insn)))
      (define rs1 (if (instr-src2 insn) (encode-gpr (instr-src2 insn)) #f))
      (define csr (encode-csr (instr-src1 insn)))
      (define zimm (instr-imm insn))
      (case op
        [(csrrw) (i-type csr rs1 #b001 rd #b1110011)]
        [(csrrs) (i-type csr rs1 #b010 rd #b1110011)]
        [(csrrc) (i-type csr rs1 #b011 rd #b1110011)]

        [(csrrwi) (i-type csr zimm #b101 rd #b1110011)]
        [(csrrsi) (i-type csr zimm #b110 rd #b1110011)]
        [(csrrci) (i-type csr zimm #b111 rd #b1110011)]
      )]

    [(add sub sll slt sltu xor srl sra or and
      mul mulh mulhsu mulhu div divu rem remu
      mulw divw divuw remw remuw
      addw subw sllw srlw sraw
      addi slti sltiu xori ori andi slli srli srai
      addiw slliw srliw sraiw
      jal jalr beq bne blt bge bltu bgeu
      lui auipc
      lb lbu lh lhu lw lwu ld sb sh sw sd)

      (define rd (if (instr-dst insn) (encode-gpr (instr-dst insn)) #f))
      (define rs1 (if (instr-src1 insn) (encode-gpr (instr-src1 insn)) #f))
      (define rs2 (if (instr-src2 insn) (encode-gpr (instr-src2 insn)) #f))
      (define imm (instr-imm insn))

      (case op
        [(add)  (r-type #b0000000 rs2 rs1 #b000 rd #b0110011)]
        [(sub)  (r-type #b0100000 rs2 rs1 #b000 rd #b0110011)]
        [(sll)  (r-type #b0000000 rs2 rs1 #b001 rd #b0110011)]
        [(slt)  (r-type #b0000000 rs2 rs1 #b010 rd #b0110011)]
        [(sltu) (r-type #b0000000 rs2 rs1 #b011 rd #b0110011)]
        [(xor)  (r-type #b0000000 rs2 rs1 #b100 rd #b0110011)]
        [(srl)  (r-type #b0000000 rs2 rs1 #b101 rd #b0110011)]
        [(sra)  (r-type #b0100000 rs2 rs1 #b101 rd #b0110011)]
        [(or)   (r-type #b0000000 rs2 rs1 #b110 rd #b0110011)]
        [(and)  (r-type #b0000000 rs2 rs1 #b111 rd #b0110011)]

        [(mul)    (r-type #b0000001 rs2 rs1 #b000 rd #b0110011)]
        [(mulh)   (r-type #b0000001 rs2 rs1 #b001 rd #b0110011)]
        [(mulhsu) (r-type #b0000001 rs2 rs1 #b010 rd #b0110011)]
        [(mulhu)  (r-type #b0000001 rs2 rs1 #b011 rd #b0110011)]
        [(div)    (r-type #b0000001 rs2 rs1 #b100 rd #b0110011)]
        [(divu)   (r-type #b0000001 rs2 rs1 #b101 rd #b0110011)]
        [(rem)    (r-type #b0000001 rs2 rs1 #b110 rd #b0110011)]
        [(remu)   (r-type #b0000001 rs2 rs1 #b111 rd #b0110011)]

        [(mulw)  (r-type #b0000001 rs2 rs1 #b000 rd #b0111011)]
        [(divw)  (r-type #b0000001 rs2 rs1 #b100 rd #b0111011)]
        [(divuw) (r-type #b0000001 rs2 rs1 #b101 rd #b0111011)]
        [(remw)  (r-type #b0000001 rs2 rs1 #b110 rd #b0111011)]
        [(remuw) (r-type #b0000001 rs2 rs1 #b111 rd #b0111011)]

        [(addw)  (r-type #b0000000 rs2 rs1 #b000 rd #b0111011)]
        [(subw)  (r-type #b0100000 rs2 rs1 #b000 rd #b0111011)]
        [(sllw)  (r-type #b0000000 rs2 rs1 #b001 rd #b0111011)]
        [(srlw)  (r-type #b0000000 rs2 rs1 #b101 rd #b0111011)]
        [(sraw)  (r-type #b0100000 rs2 rs1 #b101 rd #b0111011)]

        [(addi)  (i-type imm rs1 #b000 rd #b0010011)]
        [(slti)  (i-type imm rs1 #b010 rd #b0010011)]
        [(sltiu) (i-type imm rs1 #b011 rd #b0010011)]
        [(xori)  (i-type imm rs1 #b100 rd #b0010011)]
        [(ori)   (i-type imm rs1 #b110 rd #b0010011)]
        [(andi)  (i-type imm rs1 #b111 rd #b0010011)]
        [(slli)  (i-type imm rs1 #b001 rd #b0010011)]
        [(srli)  (i-type imm rs1 #b101 rd #b0010011)]
        [(srai)  (i-type (bvor (bv 1024 12) imm) rs1 #b101 rd #b0010011)]

        [(addiw) (i-type imm rs1 #b000 rd #b0011011)]
        [(slliw) (i-type imm rs1 #b001 rd #b0011011)]
        [(srliw) (i-type imm rs1 #b101 rd #b0011011)]
        [(sraiw) (i-type (bvor (bv 1024 12) imm) rs1 #b101 rd #b0011011)]

        [(lui)   (u-type imm rd #b0110111)]
        [(auipc) (u-type imm rd #b0010111)]

        [(jal)  (j-type imm rd #b1101111)]
        [(jalr) (i-type imm rs1 #b000 rd #b1100111)]

        [(beq)  (b-type imm rs2 rs1 #b000 #b1100011)]
        [(bne)  (b-type imm rs2 rs1 #b001 #b1100011)]
        [(blt)  (b-type imm rs2 rs1 #b100 #b1100011)]
        [(bge)  (b-type imm rs2 rs1 #b101 #b1100011)]
        [(bltu) (b-type imm rs2 rs1 #b110 #b1100011)]
        [(bgeu) (b-type imm rs2 rs1 #b111 #b1100011)]

        [(lb)  (i-type imm rs1 #b000 rd #b0000011)]
        [(lbu) (i-type imm rs1 #b100 rd #b0000011)]
        [(lh)  (i-type imm rs1 #b001 rd #b0000011)]
        [(lhu) (i-type imm rs1 #b101 rd #b0000011)]
        [(lw)  (i-type imm rs1 #b010 rd #b0000011)]
        [(lwu) (i-type imm rs1 #b110 rd #b0000011)]
        [(ld)  (i-type imm rs1 #b011 rd #b0000011)]

        [(sb) (s-type imm rs2 rs1 #b000 #b0100011)]
        [(sh) (s-type imm rs2 rs1 #b001 #b0100011)]
        [(sw) (s-type imm rs2 rs1 #b010 #b0100011)]
        [(sd) (s-type imm rs2 rs1 #b011 #b0100011)]
      )]
    [else (error (format "Encoding does not exist for ~a\n" insn)) #f]))

(provide encode-instr)
