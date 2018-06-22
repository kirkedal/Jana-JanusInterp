/* Translated from Janus program */
#include <stdio.h>      /* printf */
#include <assert.h>


void fib_forward(int &x1, int &x2, int &n);
void fib_reverse(int &x1, int &x2, int &n);

void fib_forward(int &x1, int &x2, int &n) {
    if (n == 0) {
        x1 += 1;
        x2 += 1;
        assert(x1 == x2);
    }
    else {
        n -= 1;
        fib_forward(x1, x2, n);
        x1 += x2;
        x1 ^= x2;
        x2 ^= x1;
        x1 ^= x2;
        assert(!(x1 == x2));
    }
}
void fib_reverse(int &x1, int &x2, int &n) {
    if (x1 == x2) {
        x2 -= 1;
        x1 -= 1;
        assert(n == 0);
    }
    else {
        x1 ^= x2;
        x2 ^= x1;
        x1 ^= x2;
        x1 -= x2;
        fib_reverse(x1, x2, n);
        n += 1;
        assert(!(n == 0));
    }
}

int main() {
    int x1 = 0;
    int x2 = 0;
    int n = 0;
    
    n += 5;
    fib_forward(x1, x2, n);
    printf("%d %d %d\n", n, x1, x2);
    return 1;
}
