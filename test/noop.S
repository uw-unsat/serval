
.option norvc

test:
        nop
        wfi
        nop
        sfence.vma
        fence
        fence   i, o
        fence   r, w
        fence.i
        lui     x10, 1234
        auipc   x11, 0x12

        beq     x4, x4, 3f
     3: nop

        li      x4, 123455678834

        srl     x4, x4, x5
        srli    x1, x2, 9
        srliw   x2, x31, 9

        sll     x4, x4, x5
        slli    x1, x2, 3
        slliw   x1, x2, 3

        sra     x4, x4, x5
        srai    x4, x4, 4
        sraiw   x6, x9, 12

        or      x4, x4, x6
        ori     x4, x4, 5
        and     x4, x4, x6
        andi    x4, x4, 5
        xor     x4, x4, x6
        xori    x4, x4, 5
        add     x2, x3, x4
        addi    x3, x6, 9
        addiw   x3, x6, 5

        sub     x2, x9, x10

        bltz    x1, 1f
        addi    x1, x1, 10
1:      j 2f
        addi    x1, x1, 2
        call foo
        uret
        sret
2 :     mret

        csrw    sscratch, x1
        csrr    x1, sscratch
        csrrw   x4, sscratch, x10
        csrc    sscratch, x9
        csrs    sscratch, x14
        csrwi   sscratch, 5
        csrsi   sscratch, 4
        csrci   sscratch, 3

        // Store
        sd      x1, 8(x5)
        sw      x2, 12(x6)
        sh      x3, 6(x7)
        sb      x4, 3(x8)

        // Load
        ld      x1, 8(x5)
        lwu      x2, 12(x6)
        lw      x3, 6(x7)
        lhu      x4, 3(x8)
        lh      x4, 3(x8)
        lbu      x4, 3(x8)
        lb      x4, 3(x8)

        slti   x1, x1, 5
        sltiu  x1, x1, 5

        slt   x1, x1, x2
        sltu  x1, x1, x2

        beq x1, x2, foo
        bne x1, x2, foo
        blt x1, x2, foo
        bge x1, x2, foo
        bltu x1, x2, foo
        bgeu x1, x2, foo

        mul x1, x2, x3
        mulh x1, x2, x3
        mulhsu x1, x2, x3
        mulhu x1, x2, x3
        div x1, x2, x3
        divu x1, x2, x3
        rem x1, x2, x3
        remu x1, x2, x3

        mulw x1, x2, x3
        divw x1, x2, x3
        divuw x1, x2, x3
        remw x1, x2, x3
        remuw x1, x2, x3

        addw x1, x2, x3
        subw x1, x2, x3
        sllw x1, x2, x3
        srlw x1, x2, x3
        sraw x1, x2, x3

foo:
        ret
