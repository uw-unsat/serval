
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

        csrw    sscratch, x1
        csrr    x1, sscratch
        csrrw   x4, sscratch, x10
        csrc    sscratch, x9
        csrs    sscratch, x14
        csrwi   sscratch, 5
        csrsi   sscratch, 4
        csrci   sscratch, 3

        bltz    x1, 1f
        addi    x1, x1, 10
1:      j 2f
        addi    x1, x1, 2
        call foo
2 :     mret


foo:
        ret
