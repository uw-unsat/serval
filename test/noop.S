
/*
 * A dummy file that runs through decoding for
 * each instruction opcode.
 */


/* Disable rvc except where we want it. */
.option norvc

test:

#define B(op) \
        op x1, x2, 1f; \
        1: nop; \

        B(beq)
        B(bne)
        B(blt)
        B(bge)
        B(bltu)
        B(bgeu)

#define R(op) op x1, x2, x3;

        R(add)
        R(addw)
        R(and)
        R(div)
        R(divu)
        R(divuw)
        R(divw)
        R(mul)
        R(mulh)
        R(mulhsu)
        R(mulhu)
        R(mulw)
        R(or)
        R(rem)
        R(remu)
        R(remuw)
        R(remw)
        R(sll)
        R(sllw)
        R(slt)
        R(sltu)
        R(sra)
        R(sraw)
        R(srl)
        R(srlw)
        R(sub)
        R(subw)
        R(xor)

#define I(op) op x1, x2, 0x12;

        I(addi)
        I(andi)
        I(ori)
        I(slli)
        I(slliw)
        I(slti)
        I(sltiu)
        I(srai)
        I(sraiw)
        I(srliw)
        I(xori)

#define U(op) op x1, 1234

        U(auipc)
        U(lui)

/* jal and jalr */

        jal x1, foo
foo:    nop

        la x2, bar
        jalr x1, x2
bar:    nop

/* Compressed instructions */

.option rvc

#define CR(op) op a0, a1
#define CI(op) op a0, 7
#define CB(op) \
        op a0, 1f; \
        1: \

        CR(c.addw)
        CR(c.and)
        CR(c.or)
        CR(c.sub)
        CR(c.subw)
        CR(c.xor)

        CI(c.andi)

        la a2, bar_c
        c.jr a2
bar_c:

        CR(c.mv)
        c.ebreak

        la a2, foo_c
        c.jalr a2
foo_c:

        c.add a0, a1

        CB(c.beqz)
        CB(c.bnez)

.option norvc

/* Other system instructions */

        fence
        fence i, o
        fence r, w
        sfence.vma

/* Kill interpreter using mret, nothing after will be executed, only
 * decoded. */

        mret
        sret
        uret

/* CSR */

        csrw    sscratch, x1
        csrr    x1, sscratch
        csrrw   x4, sscratch, x10
        csrc    sscratch, x9
        csrs    sscratch, x14
        csrwi   sscratch, 5
        csrsi   sscratch, 4
        csrci   sscratch, 3

#define S(op) op x1, 8(x2)

        S(lb)
        S(lbu)
        S(ld)
        S(lh)
        S(lhu)
        S(lw)
        S(lwu)
        S(sb)
        S(sd)
        S(sh)
        S(sw)
