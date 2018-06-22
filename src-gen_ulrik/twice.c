/* Translated from Janus program */
//#include <stdio.h>      /* printf */
#include <assert.h>

void dispatch_apply_forward(int *heap, int &heap_counter, int &this, int &var_var, int &var_value) {
    if (heap[this][0] == 3) {
        Sub_apply_forward(heap, heap_counter, this, var_var, var_value);
        assert(heap[this][0] == 3);
    }
    else {
        if (heap[this][0] == 2) {
            Add_apply_forward(heap, heap_counter, this, var_var, var_value);
            assert(heap[this][0] == 2);
        }
        else {
            if (heap[this][0] == 1) {
                Operator_apply_forward(heap, heap_counter, this, var_var, var_value);
                assert(heap[this][0] == 1);
            }
            else {
                if (heap[this][0] == 4) {
                    Twice_apply_forward(heap, heap_counter, this, var_var, var_value);
                    assert(heap[this][0] == 4);
                }
                else {
                    printf("Method not found");
                    exit();
                    assert(!(heap[this][0] == 4));
                }
                assert(!(heap[this][0] == 1));
            }
            assert(!(heap[this][0] == 2));
        }
        assert(!(heap[this][0] == 3));
    }
}
void dispatch_apply_reverse(int *heap, int &heap_counter, int &this, int &var_var, int &var_value) {
    if (heap[this][0] == 3) {
        Sub_apply_reverse(heap, heap_counter, this, var_var, var_value);
        assert(heap[this][0] == 3);
    }
    else {
        if (heap[this][0] == 2) {
            Add_apply_reverse(heap, heap_counter, this, var_var, var_value);
            assert(heap[this][0] == 2);
        }
        else {
            if (heap[this][0] == 1) {
                Operator_apply_reverse(heap, heap_counter, this, var_var, var_value);
                assert(heap[this][0] == 1);
            }
            else {
                if (heap[this][0] == 4) {
                    Twice_apply_reverse(heap, heap_counter, this, var_var, var_value);
                    assert(heap[this][0] == 4);
                }
                else {
                    printf("Method not found");
                    exit();
                    assert(!(heap[this][0] == 4));
                }
                assert(!(heap[this][0] == 1));
            }
            assert(!(heap[this][0] == 2));
        }
        assert(!(heap[this][0] == 3));
    }
}

void constructor_Operator_forward(int *heap, int &heap_counter, int &this) {
    heap[this][0] += 1;
}
void constructor_Operator_reverse(int *heap, int &heap_counter, int &this) {
    heap[this][0] -= 1;
}

void constructor_Add_forward(int *heap, int &heap_counter, int &this) {
    heap[this][0] += 2;
}
void constructor_Add_reverse(int *heap, int &heap_counter, int &this) {
    heap[this][0] -= 2;
}

void Add_apply_forward(int *heap, int &heap_counter, int &this, int &var_var, int &var_value) {
    var_var += var_value;
}
void Add_apply_reverse(int *heap, int &heap_counter, int &this, int &var_var, int &var_value) {
    var_var -= var_value;
}

void constructor_Sub_forward(int *heap, int &heap_counter, int &this) {
    heap[this][0] += 3;
}
void constructor_Sub_reverse(int *heap, int &heap_counter, int &this) {
    heap[this][0] -= 3;
}

void Sub_apply_forward(int *heap, int &heap_counter, int &this, int &var_var, int &var_value) {
    var_var -= var_value;
}
void Sub_apply_reverse(int *heap, int &heap_counter, int &this, int &var_var, int &var_value) {
    var_var += var_value;
}

void constructor_Twice_forward(int *heap, int &heap_counter, int &this, int &var_op) {
    heap[this][0] += 4;
    heap[this][1] += var_op;
}
void constructor_Twice_reverse(int *heap, int &heap_counter, int &this, int &var_op) {
    heap[this][1] -= var_op;
    heap[this][0] -= 4;
}

void Twice_apply_forward(int *heap, int &heap_counter, int &this, int &var_var, int &var_value) {
    int var_op = heap[this][1];
    dispatch_apply_forward(heap, heap_counter, var_op, var_var, var_value);
    dispatch_apply_forward(heap, heap_counter, var_op, var_var, var_value);
    assert(var_op == heap[this][1]);
}
void Twice_apply_reverse(int *heap, int &heap_counter, int &this, int &var_var, int &var_value) {
    int var_op = heap[this][1];
    dispatch_apply_reverse(heap, heap_counter, var_op, var_var, var_value);
    dispatch_apply_reverse(heap, heap_counter, var_op, var_var, var_value);
    assert(var_op == heap[this][1]);
}

int main() {
    int heap[10]
            [2] = {0};
    int heap_counter = 1;
    int var_a = 0;
    int var_b = 0;
    int var_aa = 0;
    int var_bb = 0;
    int var_x = 0;
    
    var_a += heap_counter;
    constructor_Add_forward(heap, heap_counter, heap_counter);
    heap_counter += 1;
    var_b += heap_counter;
    constructor_Sub_forward(heap, heap_counter, heap_counter);
    heap_counter += 1;
    var_aa += heap_counter;
    constructor_Twice_forward(heap, heap_counter, heap_counter, var_a);
    heap_counter += 1;
    var_bb += heap_counter;
    constructor_Twice_forward(heap, heap_counter, heap_counter, var_b);
    heap_counter += 1;
    int _parse_tmp_0 = 4;
    dispatch_apply_forward(heap, heap_counter, var_aa, var_x, _parse_tmp_0);
    assert(_parse_tmp_0 == 4);
    int _parse_tmp_1 = 1;
    dispatch_apply_reverse(heap, heap_counter, var_bb, var_x, _parse_tmp_1);
    assert(_parse_tmp_1 == 1);
    var_x -= 10;
    constructor_Twice_reverse(heap, heap_counter, var_bb, var_b);
    heap_counter -= 1;
    var_bb -= heap_counter;
    constructor_Twice_reverse(heap, heap_counter, var_aa, var_a);
    heap_counter -= 1;
    var_aa -= heap_counter;
    constructor_Sub_reverse(heap, heap_counter, var_b);
    heap_counter -= 1;
    var_b -= heap_counter;
    constructor_Add_reverse(heap, heap_counter, var_a);
    heap_counter -= 1;
    var_a -= heap_counter;
    return 1;
}
