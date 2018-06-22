/* Translated from Janus program */
#include <stdio.h>      /* printf */
#include <assert.h>
#include <new>
#include <cstdlib>

void crout_forward(int **A, int **LDU, int &n);
void crout_reverse(int **A, int **LDU, int &n);

void mult_forward(int &m1, int &m2);
void mult_reverse(int &m1, int &m2);

void multLD_forward(int **LDU, int **B, int &n);
void multLD_reverse(int **LDU, int **B, int &n);

void multU_forward(int **LDU, int **B, int &n);
void multU_reverse(int **LDU, int **B, int &n);

void print_2Darray(int i, int j, int **A);
void print_array(int i, int *A);

void crout_forward(int **A, int **LDU, int &n) {
    for (int j = 0 ; j != n ; j += 1) {
        for (int i = j ; i != n ; i += 1) {
            LDU[i][j] += A[i][j];
            for (int k = 0 ; k != j ; k += 1) {
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
        for (int i = j + 1 ; i != n ; i += 1) {
            LDU[j][i] += A[j][i];
            for (int k = 0 ; k != j ; k += 1) {
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
void crout_reverse(int **A, int **LDU, int &n) {
    int j = n;
    assert(j == n);
    while (!(j == 0)) {
        j -= 1;
        int i = n;
        assert(i == n);
        while (!(i == j + 1)) {
            i -= 1;
            int t = LDU[j][i];
            LDU[j][i] += t * LDU[j][j] - t;
            assert(t == LDU[j][i] / LDU[j][j]);
            int k = j;
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
            assert(k == 0);
            LDU[j][i] -= A[j][i];
            assert(!(i == n));
        }
        assert(i == j + 1);
        i = n;
        assert(i == n);
        while (!(i == j)) {
            i -= 1;
            int k = j;
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
            assert(k == 0);
            LDU[i][j] -= A[i][j];
            assert(!(i == n));
        }
        assert(i == j);
        assert(!(j == n));
    }
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

void multLD_forward(int **LDU, int **B, int &n) {
    for (int i = n ; i != 0 ; i += -1) {
            i -= 1;
            for (int j = 0 ; j != n ; j += 1) {
                    int t = B[i][j];
                    B[i][j] += t * LDU[i][i] - t;
                    assert(t == B[i][j] / LDU[i][i]);
                    for (int k = i ; k != 0 ; k += -1) {
                            k -= 1;
                            B[i][j] += LDU[k][i] * B[k][j];
                            k += 1;
                        }
                }
            i += 1;
        }
}
void multLD_reverse(int **LDU, int **B, int &n) {
    int i = 0;
    assert(i == 0);
    while (!(i == n)) {
        i -= -1;
        i -= 1;
        int j = n;
        assert(j == n);
        while (!(j == 0)) {
            j -= 1;
            int k = 0;
            assert(k == 0);
            while (!(k == i)) {
                k -= -1;
                k -= 1;
                B[i][j] -= LDU[k][i] * B[k][j];
                k += 1;
                assert(!(k == 0));
            }
            assert(k == i);
            int t = B[i][j] / LDU[i][i];
            B[i][j] -= t * LDU[i][i] - t;
            assert(t == B[i][j]);
            assert(!(j == n));
        }
        assert(j == 0);
        i += 1;
        assert(!(i == 0));
    }
    assert(i == n);
}


void multU_forward(int **LDU, int **B, int &n) {
    for (int i = 0 ; i != n ; i += 1) {
        for (int j = 0 ; j != n ; j += 1) {
            for (int k = i + 1 ; k != n ; k += 1) {
                B[i][j] += LDU[k][i] * B[k][j];
            }
        }
    }
}
void multU_reverse(int **LDU, int **B, int &n) {
    int i = n;
    assert(i == n);
    while (!(i == 0)) {
        i -= 1;
        int j = n;
        assert(j == n);
        while (!(j == 0)) {
            j -= 1;
            int k = n;
            assert(k == n);
            while (!(k == i + 1)) {
                k -= 1;
                B[i][j] -= LDU[k][i] * B[k][j];
                assert(!(k == n));
            }
            assert(k == i + 1);
            assert(!(j == n));
        }
        assert(j == 0);
        assert(!(i == n));
    }
    assert(i == 0);
}


void print_array(int length, int *intarray) {
  printf("[");
  for (int i = 0 ; i < length - 1 ; i++) {
    if (intarray[i] < 10) {
      printf(" ");
    }
    printf("%d, ", intarray[i]);
  }
  if (intarray[length - 1] < 10) {
    printf(" ");
  }
  printf("%d", intarray[length - 1]);
  printf("]\n");
}

void print_2Darray(int length_i, int length_j, int **intarray) {
  printf("[");
  for (int i = 0 ; i < length_i - 1 ; i++) {
    print_array(length_j, intarray[i]);
    printf(" ");
  }
  print_array(length_j, intarray[length_i - 1]);
  printf("]\n");
}


// void mmult(A)

int main() {
    int n = 0;
    n += 3;

    int **A;
    int **B;
    int **LDU;
    A = new int *[3];
    B = new int *[3];
    LDU = new int *[3];
    for(int i = 0; i <3; i++) {
        A[i] = new int[3];
        B[i] = new int[3];
        LDU[i] = new int[3];
        for(int j = 0; j <3; j++) {
            A[i][j] = 0;
            B[i][j] = 0;
            LDU[i][j] = 0;
        }
    }

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

    // print_2Darray(3, 3, A);
    crout_forward(A, LDU, n);
    multLD_forward(LDU, B, n);
    multU_forward(LDU, B, n);
    crout_reverse(A, LDU, n);
    print_2Darray(3, 3, B);
    print_2Darray(3, 3, LDU);

    return 1;
}
