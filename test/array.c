#include <stdint.h>

struct A { uint32_t x, y; } as[4];
uint32_t arr[4];

uint32_t test(uint32_t x)
{
    uint32_t idx = x % 4;

    as[idx].y = 9;
    arr[(idx + 1) % 4] = as[idx].y;
    return arr[(idx + 1) % 4];
}
