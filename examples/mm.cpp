/* Translated from Janus program */
#include <stdio.h>      /* printf */
#include <assert.h>


void crout_forward(int *A, int *LDU, int &n);
void crout_reverse(int *A, int *LDU, int &n);

void mult_forward(int &m1, int &m2);
void mult_reverse(int &m1, int &m2);

void multLD_forward(int *LDU, int *B, int &n);
void multLD_reverse(int *LDU, int *B, int &n);

void multU_forward(int *LDU, int *B, int &n);
void multU_reverse(int *LDU, int *B, int &n);

void crout_forward(int *A, int *LDU, int &n) {
    int j = 0;
    int i = 0;
    int k = 0;
    assert(j == 0);
    while (!(j == n)) {
        i += j;
        assert(i == j);
        while (!(i == n)) {
            LDU[i][j] += A[i][j];
            assert(k == 0);
            while (!(k == j)) {
                if (k == j) {
                    LDU[i][j] -= LDU[i][k];
                    assert(k == j);
                }
                else {
                    LDU[i][j] -= LDU[i][k] * LDU[k][j];
                    assert(!(k == j));
                }
                k += 1;
                assert(!(k == 0));
            }
            k -= j;
            i += 1;
            assert(!(i == j));
        }
        i -= n;
        i += j + 1;
        assert(i == j + 1);
        while (!(i == n)) {
            LDU[j][i] += A[j][i];
            assert(k == 0);
            while (!(k == j)) {
                if (k == i) {
                    LDU[j][j] -= LDU[j][k];
                    assert(k == j);
                }
                else {
                    LDU[j][i] -= LDU[j][k] * LDU[k][i];
                    assert(!(k == j));
                }
                k += 1;
                assert(!(k == 0));
            }
            k -= j;
            int t = LDU[j][i] / LDU[j][j];
            LDU[j][i] -= t * LDU[j][j] - t;
            assert(t == LDU[j][i]);
            i += 1;
            assert(!(i == j + 1));
        }
        i -= n;
        j += 1;
        assert(!(j == 0));
    }
    j -= n;
    assert(k == 0);
    assert(i == 0);
    assert(j == 0);
}
void crout_reverse(int *A, int *LDU, int &n) {
    int j = 0;
    int i = 0;
    int k = 0;
    j += n;
    assert(j == n);
    while (!(j == 0)) {
        j -= 1;
        i += n;
        assert(i == n);
        while (!(i == j + 1)) {
            i -= 1;
            int t = LDU[j][i];
            LDU[j][i] += t * LDU[j][j] - t;
            assert(t == LDU[j][i] / LDU[j][j]);
            k += j;
            assert(k == j);
            while (!(k == 0)) {
                k -= 1;
                if (k == j) {
                    LDU[j][j] += LDU[j][k];
                    assert(k == i);
                }
                else {
                    LDU[j][i] += LDU[j][k] * LDU[k][i];
                    assert(!(k == i));
                }
                assert(!(k == j));
            }
            LDU[j][i] -= A[j][i];
            assert(!(i == n));
        }
        i -= j + 1;
        i += n;
        assert(i == n);
        while (!(i == j)) {
            i -= 1;
            k += j;
            assert(k == j);
            while (!(k == 0)) {
                k -= 1;
                if (k == j) {
                    LDU[i][j] += LDU[i][k];
                    assert(k == j);
                }
                else {
                    LDU[i][j] += LDU[i][k] * LDU[k][j];
                    assert(!(k == j));
                }
                assert(!(k == j));
            }
            LDU[i][j] -= A[i][j];
            assert(!(i == n));
        }
        i -= j;
        assert(!(j == n));
    }
    assert(k == 0);
    assert(i == 0);
    assert(j == 0);
}

void mult_forward(int &m1, int &m2) {
    int t = m1;
    printf("B m1 %d m2 %d t %d\n", m1, m2, t);
    m1 += t * m2 - t;
    printf("A m1 %d m2 %d t %d\n", m1, m2, t);
    assert(t == m1 / m2);
}
void mult_reverse(int &m1, int &m2) {
    int t = m1 / m2;
    printf("A m1 %d m2 %d t %d\n", m1, m2, t);
    m1 -= t * m2 - t;
    printf("B m1 %d m2 %d t %d\n", m1, m2, t);
    assert(t == m1);
}

void multLD_forward(int *LDU, int *B, int &n) {
    int i = 0;
    int j = 0;
    int k = 0;
    i += n;
    assert(i == n);
    i -= 1;
    assert(j == 0);
    while (!(j == n)) {
        int t = B[i][j];
        B[i][j] += t * LDU[i][i] - t;
        assert(t == B[i][j] / LDU[i][i]);
        k += i;
        assert(k == i);
        while (!(k == 0)) {
            k -= 1;
            B[i][j] += LDU[k][i] * B[k][j];
            assert(!(k == i));
        }
        j += 1;
        assert(!(j == 0));
    }
    j -= n;
    while (!(i == 0)) {
        assert(!(i == n));
        i -= 1;
        assert(j == 0);
        while (!(j == n)) {
            int t = B[i][j];
            B[i][j] += t * LDU[i][i] - t;
            assert(t == B[i][j] / LDU[i][i]);
            k += i;
            assert(k == i);
            while (!(k == 0)) {
                k -= 1;
                B[i][j] += LDU[k][i] * B[k][j];
                assert(!(k == i));
            }
            j += 1;
            assert(!(j == 0));
        }
        j -= n;
    }
    assert(k == 0);
    assert(j == 0);
    assert(i == 0);
}
void multLD_reverse(int *LDU, int *B, int &n) {
    int i = 0;
    int j = 0;
    int k = 0;
    assert(i == 0);
    j += n;
    assert(j == n);
    while (!(j == 0)) {
        j -= 1;
        assert(k == 0);
        while (!(k == i)) {
            B[i][j] -= LDU[k][i] * B[k][j];
            k += 1;
            assert(!(k == 0));
        }
        k -= i;
        int t = B[i][j] / LDU[i][i];
        B[i][j] -= t * LDU[i][i] - t;
        assert(t == B[i][j]);
        assert(!(j == n));
    }
    i += 1;
    while (!(i == n)) {
        assert(!(i == 0));
        j += n;
        assert(j == n);
        while (!(j == 0)) {
            j -= 1;
            assert(k == 0);
            while (!(k == i)) {
                B[i][j] -= LDU[k][i] * B[k][j];
                k += 1;
                assert(!(k == 0));
            }
            k -= i;
            int t = B[i][j] / LDU[i][i];
            B[i][j] -= t * LDU[i][i] - t;
            assert(t == B[i][j]);
            assert(!(j == n));
        }
        i += 1;
    }
    i -= n;
    assert(k == 0);
    assert(j == 0);
    assert(i == 0);
}

void multU_forward(int *LDU, int *B, int &n) {
    int i = 0;
    int j = 0;
    int k = 0;
    assert(i == 0);
    assert(j == 0);
    while (!(j == n)) {
        k += i + 1;
        assert(k == i + 1);
        while (!(k == n)) {
            B[i][j] += LDU[k][i] * B[k][j];
            k += 1;
            assert(!(k == i + 1));
        }
        k -= n;
        j += 1;
        assert(!(j == 0));
    }
    j -= n;
    i += 1;
    while (!(i == n)) {
        assert(!(i == 0));
        assert(j == 0);
        while (!(j == n)) {
            k += i + 1;
            assert(k == i + 1);
            while (!(k == n)) {
                B[i][j] += LDU[k][i] * B[k][j];
                k += 1;
                assert(!(k == i + 1));
            }
            k -= n;
            j += 1;
            assert(!(j == 0));
        }
        j -= n;
        i += 1;
    }
    i -= n;
    assert(k == 0);
    assert(j == 0);
    assert(i == 0);
}
void multU_reverse(int *LDU, int *B, int &n) {
    int i = 0;
    int j = 0;
    int k = 0;
    i += n;
    assert(i == n);
    i -= 1;
    j += n;
    assert(j == n);
    while (!(j == 0)) {
        j -= 1;
        k += n;
        assert(k == n);
        while (!(k == i + 1)) {
            k -= 1;
            B[i][j] -= LDU[k][i] * B[k][j];
            assert(!(k == n));
        }
        k -= i + 1;
        assert(!(j == n));
    }
    while (!(i == 0)) {
        assert(!(i == n));
        i -= 1;
        j += n;
        assert(j == n);
        while (!(j == 0)) {
            j -= 1;
            k += n;
            assert(k == n);
            while (!(k == i + 1)) {
                k -= 1;
                B[i][j] -= LDU[k][i] * B[k][j];
                assert(!(k == n));
            }
            k -= i + 1;
            assert(!(j == n));
        }
    }
    assert(k == 0);
    assert(j == 0);
    assert(i == 0);
}

int main() {
    int A[3]
         [3] = {0};
    int B[3]
         [3] = {0};
    int LDU[3]
           [3] = {0};
    int n = 0;
    int x = 0;
    int y = 0;
    
    n += 3;
    x += 2;
    y += 4;
    A[0][0] ^= 2;
    A[1][0] ^= 4;
    A[2][0] ^= 2;
    A[0][1] ^= 4;
    A[1][1] ^= 1;
    A[2][1] ^= 3;
    A[0][2] ^= 4;
    A[1][2] ^= 1;
    A[2][2] ^= 4;
    B[0][0] ^= 3;
    B[1][0] ^= 1;
    B[2][0] ^= 4;
    B[0][1] ^= 2;
    B[1][1] ^= 2;
    B[2][1] ^= 3;
    B[0][2] ^= 4;
    B[1][2] ^= 1;
    B[2][2] ^= 1;
    crout_forward(A, LDU, n);
    multLD_forward(LDU, B, n);
    multU_forward(LDU, B, n);
    crout_reverse(A, LDU, n);
    return 1;
}
