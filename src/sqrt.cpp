/* Translated from Janus program */
#include <stdio.h>      /* printf */
#include <assert.h>

void doublebit_forward(int &bit) {
    int z = bit;
    bit += z;
    assert(z == bit / 2);
}
void doublebit_reverse(int &bit) {
    int z = bit / 2;
    bit -= z;
    assert(z == bit);
}

void root_forward(int &num, int &root) {
    int bit = 1;
    assert(bit == 1);
    while (!(bit * bit > num)) {
        doublebit_forward(bit);
        assert(!(bit == 1));
    }
    assert(bit * bit > num);
    doublebit_reverse(bit);
    if ( (root + bit) * (root + bit) <= num ) {
        root += bit;
        assert(root / bit % 2 != 0);
    }
    while (!(bit == 1)) {
        assert(!(bit * bit > num));
        doublebit_reverse(bit);
        if ( (root + bit) * (root + bit) <= num ) {
            root += bit;
            assert(root / bit % 2 != 0);
        }
    }
    assert(bit == 1);
    num -= root * root;
}
void root_reverse(int &num, int &root) {
    num += root * root;
    int bit = 1;
    assert(bit == 1);
    if ( root / bit % 2 != 0 ) {
        root -= bit;
        assert((root + bit) * (root + bit) <= num);
    }
    doublebit_forward(bit);
    while (!(bit * bit > num)) {
        assert(!(bit == 1));
        if ( root / bit % 2 != 0 ) {
            root -= bit;
            assert((root + bit) * (root + bit) <= num);
        }
        doublebit_forward(bit);
    }
    assert(bit * bit > num);
    while (!(bit == 1)) {
        doublebit_reverse(bit);
        assert(!(bit * bit > num));
    }
    assert(bit == 1);
}

int main() {
    int num = 0;
    int root = 0;
    
    num += 66;
    printf("%d %d \n", num, root);
    root_forward(num, root);
    printf("%d %d \n", num, root);
    return 1;
}
