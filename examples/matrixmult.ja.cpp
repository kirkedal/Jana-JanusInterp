/* Translated from Janus program */
#include <stdio.h>      /* printf */
#include <assert.h>


void crout_forward(int **LDU, int &n);
void crout_reverse(int **LDU, int &n);

void multLD_forward(int **A, int **LDU, int &n);
void multLD_reverse(int **A, int **LDU, int &n);

void multU_forward(int **A, int **LDU, int &n);
void multU_reverse(int **A, int **LDU, int &n);

void matrix_mult_forward(int **A, int **B, int &m);
void matrix_mult_reverse(int **A, int **B, int &m);

void crout_forward(int **LDU, int &n) {
    for (int j = 0 ; j != n - 1 + 1 ; j += 1) {
        for (int i = j ; i != n - 1 + 1 ; i += 1) {
            for (int k = 0 ; k != j - 1 + 1 ; k += 1) {
                if (k == j) {
                    LDU[i][j] -= LDU[i][k];
                    assert(k == j);
                }
                else {
                    LDU[i][j] -= LDU[i][k] * LDU[k][j];
                    assert(!(k == j));
                }
            }
        }
        for (int i = j + 1 ; i != n - 1 + 1 ; i += 1) {
            for (int k = 0 ; k != j - 1 + 1 ; k += 1) {
                if (k == i) {
                    LDU[j][j] -= LDU[j][k];
                    assert(k == j);
                }
                else {
                    LDU[j][i] -= LDU[j][k] * LDU[k][i];
                    assert(!(k == j));
                }
            }
            int t = LDU[j][i] / LDU[j][j];
            LDU[j][i] -= t * LDU[j][j] - t;
            assert(t == LDU[j][i]);
        }
    }
}
void crout_reverse(int **LDU, int &n) {
    for (int j = n - 1 ; j != 0 + 0 - 1 ; j += 0 - 1) {
        for (int i = n - 1 ; i != j + 1 + 0 - 1 ; i += 0 - 1) {
            int t = LDU[j][i];
            LDU[j][i] += t * LDU[j][j] - t;
            assert(t == LDU[j][i] / LDU[j][j]);
            for (int k = j - 1 ; k != 0 + 0 - 1 ; k += 0 - 1) {
                if (k == j) {
                    LDU[j][j] += LDU[j][k];
                    assert(k == i);
                }
                else {
                    LDU[j][i] += LDU[j][k] * LDU[k][i];
                    assert(!(k == i));
                }
            }
        }
        for (int i = n - 1 ; i != j + 0 - 1 ; i += 0 - 1) {
            for (int k = j - 1 ; k != 0 + 0 - 1 ; k += 0 - 1) {
                if (k == j) {
                    LDU[i][j] += LDU[i][k];
                    assert(k == j);
                }
                else {
                    LDU[i][j] += LDU[i][k] * LDU[k][j];
                    assert(!(k == j));
                }
            }
        }
    }
}

void multLD_forward(int **A, int **LDU, int &n) {
    for (int i = 0 ; i != n - 1 + 1 ; i += 1) {
        for (int j = 0 ; j != n - 1 + 1 ; j += 1) {
            int t = A[j][i];
            A[j][i] += t * LDU[i][i] - t;
            assert(t == A[j][i] / LDU[i][i]);
            for (int k = i + 1 ; k != n - 1 + 1 ; k += 1) {
                A[j][i] += LDU[k][i] * A[j][k];
            }
        }
    }
}
void multLD_reverse(int **A, int **LDU, int &n) {
    for (int i = n - 1 ; i != 0 + 0 - 1 ; i += 0 - 1) {
        for (int j = n - 1 ; j != 0 + 0 - 1 ; j += 0 - 1) {
            for (int k = n - 1 ; k != i + 1 + 0 - 1 ; k += 0 - 1) {
                A[j][i] -= LDU[k][i] * A[j][k];
            }
            int t = A[j][i] / LDU[i][i];
            A[j][i] -= t * LDU[i][i] - t;
            assert(t == A[j][i]);
        }
    }
}

void multU_forward(int **A, int **LDU, int &n) {
    for (int i = n - 1 ; i != 0 + -1 ; i += -1) {
        for (int j = 0 ; j != n - 1 + 1 ; j += 1) {
            for (int k = 0 ; k != i - 1 + 1 ; k += 1) {
                A[j][i] += LDU[k][i] * A[j][k];
            }
        }
    }
}
void multU_reverse(int **A, int **LDU, int &n) {
    for (int i = 0 ; i != n - 1 + 0 - -1 ; i += 0 - -1) {
        for (int j = n - 1 ; j != 0 + 0 - 1 ; j += 0 - 1) {
            for (int k = i - 1 ; k != 0 + 0 - 1 ; k += 0 - 1) {
                A[j][i] -= LDU[k][i] * A[j][k];
            }
        }
    }
}

void matrix_mult_forward(int **A, int **B, int &m) {
    crout_forward(B, n);
    multLD_forward(A, B, n);
    multU_forward(A, B, n);
    crout_reverse(B, n);
}
void matrix_mult_reverse(int **A, int **B, int &m) {
    crout_forward(B, n);
    multU_reverse(A, B, n);
    multLD_reverse(A, B, n);
    crout_reverse(B, n);
}

