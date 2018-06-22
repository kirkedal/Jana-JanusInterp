/* Translated from Janus program */
#include <stdio.h>      /* printf */
#include <assert.h>

void zeroi_forward(int &i, int *fact) {
    assert(i == 0);
    while (!(fact[i + 1] == 0)) {
        i += 1;
        assert(!(i == 0));
    }
}
void zeroi_reverse(int &i, int *fact) {
    assert(fact[i + 1] == 0);
    while (!(i == 0)) {
        i -= 1;
        assert(!(fact[i + 1] == 0));
    }
}

void nexttryFactor_forward(int &tryFactor) {
    if (tryFactor == 3) {
        tryFactor += 1;
        assert(tryFactor == 4);
    }
    tryFactor -= 2;
}
void nexttryFactor_reverse(int &tryFactor) {
    tryFactor += 2;
    if (tryFactor == 4) {
        tryFactor -= 1;
        assert(tryFactor == 3);
    }
}

void factor_forward(int &num, int *fact) {
    int tryFactor = 0;
    int i = 0;
    zeroi_forward(i, fact);
    if (fact[i - 1] * fact[i - 1] < fact[i]) {
        assert(tryFactor == 0);
        while (!(tryFactor * tryFactor > fact[i])) {
            nexttryFactor_reverse(tryFactor);
            assert(!(tryFactor == 0));
        }
        assert(fact[i - 1] * fact[i - 1] < fact[i]);
    }
    else {
        tryFactor += fact[i - 1];
        assert(!(fact[i - 1] * fact[i - 1] < fact[i]));
    }
    if (fact[i] != fact[i - 1]) {
        fact[i] ^= num;
        num ^= fact[i];
        fact[i] ^= num;
        i -= 1;
        assert(num != 1);
    }
    else {
        num += 1;
        assert(!(num != 1));
    }
    assert(tryFactor * tryFactor > num);
    while (!(tryFactor == 0 && num > 1)) {
        assert(num % tryFactor != 0);
        while (!(fact[i] != tryFactor)) {
            int z = num * tryFactor;
            z ^= num;
            num ^= z;
            z ^= num;
            assert(z == num / tryFactor);
            fact[i] -= tryFactor;
            i -= 1;
            assert(!(num % tryFactor != 0));
        }
        nexttryFactor_forward(tryFactor);
        assert(!(tryFactor * tryFactor > num));
    }
    assert(i == 0);
    assert(tryFactor == 0);
}
void factor_reverse(int &num, int *fact) {
    int tryFactor = 0;
    int i = 0;
    assert(tryFactor == 0 && num > 1);
    while (!(tryFactor * tryFactor > num)) {
        nexttryFactor_reverse(tryFactor);
        assert(fact[i] != tryFactor);
        while (!(num % tryFactor != 0)) {
            i += 1;
            fact[i] += tryFactor;
            int z = num / tryFactor;
            z ^= num;
            num ^= z;
            z ^= num;
            assert(z == num * tryFactor);
            assert(!(fact[i] != tryFactor));
        }
        assert(!(tryFactor == 0 && num > 1));
    }
    if (num != 1) {
        i += 1;
        fact[i] ^= num;
        num ^= fact[i];
        fact[i] ^= num;
        assert(fact[i] != fact[i - 1]);
    }
    else {
        num -= 1;
        assert(!(fact[i] != fact[i - 1]));
    }
    if (fact[i - 1] * fact[i - 1] < fact[i]) {
        assert(tryFactor * tryFactor > fact[i]);
        while (!(tryFactor == 0)) {
            nexttryFactor_forward(tryFactor);
            assert(!(tryFactor * tryFactor > fact[i]));
        }
        assert(fact[i - 1] * fact[i - 1] < fact[i]);
    }
    else {
        tryFactor -= fact[i - 1];
        assert(!(fact[i - 1] * fact[i - 1] < fact[i]));
    }
    zeroi_reverse(i, fact);
    assert(i == 0);
    assert(tryFactor == 0);
}

int main() {
    int num = 0;
    int fact[20] = {0};
    
    num += 840;
    factor_forward(num, fact);
    return 1;
}
