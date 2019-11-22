
#define N 8

typedef long (*function)(long);

function table[N];

long add0(long x) {return x + 0;}
long add1(long x) {return x + 1;}
long add2(long x) {return x + 2;}
long add3(long x) {return x + 3;}
long add4(long x) {return x + 4;}
long add5(long x) {return x + 5;}
long add6(long x) {return x + 6;}
long add7(long x) {return x + 7;}


void init_table(void)
{
        table[0] = add0;
        table[1] = add1;
        table[2] = add2;
        table[3] = add3;
        table[4] = add4;
        table[5] = add5;
        table[6] = add6;
        table[7] = add7;
}

void __attribute__((naked)) mret(void)
{
        asm volatile("mret");
}

long call_func(long x, unsigned long y) {
        y = y & 0x7;
        return table[y](x);
}
