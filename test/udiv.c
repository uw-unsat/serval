#include <stdint.h>

uint32_t udiv(uint32_t x, uint32_t y)
{
    if (y != 0)
        return x / y;
    return 0;
}

__attribute__((noinline))
static uint32_t foo(uint32_t x, uint32_t y)
{
    return x / y;
}

uint32_t udiv_buggy(uint32_t x, uint32_t y)
{
    return foo(x, y);
}
