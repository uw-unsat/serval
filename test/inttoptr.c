#include <stdint.h>

int global;
volatile int *volatile pointer;

__attribute__((noinline)) uintptr_t getglobal(void)
{
    return (uintptr_t)&global;
}

/* Try to hide pointer arithmetic. */
__attribute__((noinline)) uintptr_t add1(uintptr_t x) {return x + 1;}
__attribute__((noinline)) uintptr_t sub1(uintptr_t x) {return x - 1;}

int test1(void)
{
    uintptr_t a, b;

    a = getglobal();
    b = getglobal();

    *(volatile int *)(add1(sub1(a))) = 5;
    return *(volatile int *)(sub1(add1(b))) - 5;
}

int test2(void)
{
    global = 0x42;

    pointer = &global;

    return *pointer - 0x42;
}
