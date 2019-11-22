#include <stdint.h>

typedef uint32_t uint_xlen_t;
#define uint_xlen_t(n) ((uint_xlen_t) (n))
#define XLEN 32

uint_xlen_t bext(uint_xlen_t rs1, uint_xlen_t rs2)
{
    uint_xlen_t r = 0;
    for (int i = 0, j = 0; i < XLEN; i++)
        if ((rs2 >> i) & 1) {
            if ((rs1 >> i) & 1)
                r |= uint_xlen_t(1) << j;
            j++;
        }
    return r;
}
