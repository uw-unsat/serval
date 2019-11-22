#include <stddef.h>
#include <stdint.h>

#define N 4096

uint64_t val;
uint32_t vals[N];

uint64_t get_value(void)
{
    return val;
}

uint32_t get_value_i(size_t i)
{
    return (i < N) ? vals[i] : -1;
}

void set_value(uint64_t x)
{
    val = x;
}

void set_value_i(size_t i, uint32_t x)
{
    if (i < N)
        vals[i] = x;
}
