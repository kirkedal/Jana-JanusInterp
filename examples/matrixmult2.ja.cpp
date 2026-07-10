/* Translated from Janus program */
#include <stdio.h>      /* printf */
#include <assert.h>
#include <math.h>
#include <vector>


template <std::size_t SIZE1>
void crout_forward(std::vector<int> &LDU, int &n);
template <std::size_t SIZE1>
void crout_reverse(std::vector<int> &LDU, int &n);

template <std::size_t SIZE1, std::size_t SIZE2>
void multLD_forward(std::vector<int> &A, std::vector<int> &LDU, int &n);
template <std::size_t SIZE1, std::size_t SIZE2>
void multLD_reverse(std::vector<int> &A, std::vector<int> &LDU, int &n);

template <std::size_t SIZE1, std::size_t SIZE2>
void multU_forward(std::vector<int> &A, std::vector<int> &LDU, int &n);
template <std::size_t SIZE1, std::size_t SIZE2>
void multU_reverse(std::vector<int> &A, std::vector<int> &LDU, int &n);

void mult_forward(int &x, int &y);
void mult_reverse(int &x, int &y);

template <std::size_t SIZE1, std::size_t SIZE2>
void matrix_mult_forward(std::vector<int> &A, std::vector<int> &B, int &n);
template <std::size_t SIZE1, std::size_t SIZE2>
void matrix_mult_reverse(std::vector<int> &A, std::vector<int> &B, int &n);

void crout_forward(std::vector<int> &LDU, int &n) {
    for (int j = 0 ; j != n - 1 + 1 ; j += 1) {
        for (int i = j ; i != n - 1 + 1 ; i += 1) {
            for (int k = 0 ; k != j - 1 + 1 ; k += 1) {
                LDU[i][j] -= LDU[i][k] * LDU[k][j];
            }
        }
        for (int i = j + 1 ; i != n - 1 + 1 ; i += 1) {
            for (int k = 0 ; k != j - 1 + 1 ; k += 1) {
                LDU[j][i] -= LDU[j][k] * LDU[k][i];
            }
            mult_reverse(LDU[j][i], LDU[j][j]);
        }
    }
}
void crout_reverse(std::vector<int> &LDU, int &n) {
    for (int j = n - 1 ; j != 0 + 0 - 1 ; j += 0 - 1) {
        for (int i = n - 1 ; i != j + 1 + 0 - 1 ; i += 0 - 1) {
            mult_forward(LDU[j][i], LDU[j][j]);
            for (int k = j - 1 ; k != 0 + 0 - 1 ; k += 0 - 1) {
                LDU[j][i] += LDU[j][k] * LDU[k][i];
            }
        }
        for (int i = n - 1 ; i != j + 0 - 1 ; i += 0 - 1) {
            for (int k = j - 1 ; k != 0 + 0 - 1 ; k += 0 - 1) {
                LDU[i][j] += LDU[i][k] * LDU[k][j];
            }
        }
    }
}

void multLD_forward(std::vector<int> &A, std::vector<int> &LDU, int &n) {
    for (int i = 0 ; i != n - 1 + 1 ; i += 1) {
        for (int j = 0 ; j != n - 1 + 1 ; j += 1) {
            mult_forward(A[j][i], LDU[i][i]);
            for (int k = i + 1 ; k != n - 1 + 1 ; k += 1) {
                A[j][i] += LDU[k][i] * A[j][k];
            }
        }
    }
}
void multLD_reverse(std::vector<int> &A, std::vector<int> &LDU, int &n) {
    for (int i = n - 1 ; i != 0 + 0 - 1 ; i += 0 - 1) {
        for (int j = n - 1 ; j != 0 + 0 - 1 ; j += 0 - 1) {
            for (int k = n - 1 ; k != i + 1 + 0 - 1 ; k += 0 - 1) {
                A[j][i] -= LDU[k][i] * A[j][k];
            }
            mult_reverse(A[j][i], LDU[i][i]);
        }
    }
}

void multU_forward(std::vector<int> &A, std::vector<int> &LDU, int &n) {
    for (int i = n - 1 ; i != 0 + -1 ; i += -1) {
        for (int j = 0 ; j != n - 1 + 1 ; j += 1) {
            for (int k = 0 ; k != i - 1 + 1 ; k += 1) {
                A[j][i] += LDU[k][i] * A[j][k];
            }
        }
    }
}
void multU_forward(std::vector<int> &A, std::vector<int> &LDU, int &n) {
    for (int i = n - 1 ; i != 0 + -1 ; i += -1) {
        for (int j = 0 ; j != n - 1 + 1 ; j += 1) {
            int s = 0;
            for (int k = 0 ; k != i - 1 + 1 ; k += 1) {
                s += LDU[k][i] * A[j][k];
            }
            A[j][i] += s;
            for (int k = 0 ; k != i - 1 + 1 ; k += 1) {
                s -= LDU[k][i] * A[j][k];
            }
            s = 0;
        }
    }
}
void multU_reverse(std::vector<int> &A, std::vector<int> &LDU, int &n) {
    for (int i = 0 ; i != n - 1 + 0 - -1 ; i += 0 - -1) {
        for (int j = n - 1 ; j != 0 + 0 - 1 ; j += 0 - 1) {
            for (int k = i - 1 ; k != 0 + 0 - 1 ; k += 0 - 1) {
                A[j][i] -= LDU[k][i] * A[j][k];
            }
        }
    }
}

void mult_forward(int &x, int &y) {
    int t = x * y;
    x ^= t;
    t ^= x;
    x ^= t;
    assert(t == x / y);
}
void mult_reverse(int &x, int &y) {
    int t = x / y;
    x ^= t;
    t ^= x;
    x ^= t;
    assert(t == x * y);
}

void matrix_mult_forward(std::vector<int> &A, std::vector<int> &B, int &n) {
    crout_forward(B, n);
    multLD_forward(A, B, n);
    multU_forward(A, B, n);
    crout_reverse(B, n);
}
void matrix_mult_reverse(std::vector<int> &A, std::vector<int> &B, int &n) {
    crout_forward(B, n);
    multU_reverse(A, B, n);
    multLD_reverse(A, B, n);
    crout_reverse(B, n);
}

int main() {
    std::vector<int> A(3
                       3, 0);
    std::vector<int> B(3
                       3, 0);
    int n = 0;
    
    n += 3;
    A[0][0] ^= 3;
    A[0][1] ^= 1;
    A[0][2] ^= 4;
    A[1][0] ^= 2;
    A[1][1] ^= 2;
    A[1][2] ^= 3;
    A[2][0] ^= 4;
    A[2][1] ^= 1;
    A[2][2] ^= 1;
    B[0][0] ^= 2;
    B[0][1] ^= 4;
    B[0][2] ^= 4;
    B[1][0] ^= 4;
    B[1][1] ^= 1;
    B[1][2] ^= 1;
    B[2][0] ^= 2;
    B[2][2] ^= 3;
    B[2][2] ^= 4;
    crout_forward(B, n);
    multLD_forward(A, B, n);
    multU_forward(A, B, n);
    crout_reverse(B, n);
    return 1;
}
