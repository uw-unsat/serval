#include <stddef.h>
#include <stdint.h>

#define N 10

struct A {
    int x;
    struct B {
        int y, z;
    } b;
};

struct A a[N];

uint8_t pages[N][4096];
uint8_t buffer[N];

__attribute__((naked))
void mret(void)
{
    asm volatile("mret");
}

__attribute__((noinline))
void *memset(void *p, int c, size_t len)
{
    char *s = p;
    size_t i;

    for (i = 0; i < len; ++i)
        s[i] = c;

    return p;
}

void test_byte_buffer(void)
{
    memset(buffer, 0, N);
}

void test_a0(void)
{
    memset(a, 0, sizeof(struct A));
}

void test_ai(size_t i)
{
    if (i < N)
        memset(&a[i], 0, sizeof(struct A));
}

void test_an(void)
{
    memset(a, 0, sizeof(a));
}

void test_b(size_t i)
{
    if (i < N)
        memset(&a[i].b, 0, sizeof(struct B));
}

void test_b_z(size_t i)
{
    if (i < N)
        memset(&a[i].b.z, 0, sizeof(int));
}

void test_pages(size_t lower, size_t upper)
{
    if (lower <= upper && upper <= N)
        memset(&pages[lower], 0, (upper - lower) * 4096);
}

void test_buggy_too_large_a(void)
{
    memset(a, 0, sizeof(a) + sizeof(struct A));
}

void test_buggy_too_large_b(size_t i)
{
    memset(&a[0].b.z, 0, sizeof(struct B));
}

void test_buggy_out_of_bounds(size_t i)
{
    memset(&a[i], 0, sizeof(struct A));
}
