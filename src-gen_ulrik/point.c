/* Translated from Janus program */
#include <stdio.h>      /* printf */
#include <assert.h>

void dispatch_add_to_y_forward(int *heap, int &heap_counter, int &this, int &var_y) {
    if (heap[this
             ,
             0] == 1) {
        Point_add_to_y_forward(heap, heap_counter, this, var_y);
        assert(heap[this
                    ,
                    0] == 1);
    }
    else {
        printf("Method not found");
        exit();
        assert(!(heap[this
                      ,
                      0] == 1));
    }
}
void dispatch_add_to_y_reverse(int *heap, int &heap_counter, int &this, int &var_y) {
    if (heap[this
             ,
             0] == 1) {
        Point_add_to_y_reverse(heap, heap_counter, this, var_y);
        assert(heap[this
                    ,
                    0] == 1);
    }
    else {
        printf("Method not found");
        exit();
        assert(!(heap[this
                      ,
                      0] == 1));
    }
}

void dispatch_example1_forward(int *heap, int &heap_counter, int &this) {
    if (heap[this
             ,
             0] == 2) {
        Main_example1_forward(heap, heap_counter, this);
        assert(heap[this
                    ,
                    0] == 2);
    }
    else {
        printf("Method not found");
        exit();
        assert(!(heap[this
                      ,
                      0] == 2));
    }
}
void dispatch_example1_reverse(int *heap, int &heap_counter, int &this) {
    if (heap[this
             ,
             0] == 2) {
        Main_example1_reverse(heap, heap_counter, this);
        assert(heap[this
                    ,
                    0] == 2);
    }
    else {
        printf("Method not found");
        exit();
        assert(!(heap[this
                      ,
                      0] == 2));
    }
}

void dispatch_add_to_x_forward(int *heap, int &heap_counter, int &this, int &var_x) {
    if (heap[this
             ,
             0] == 1) {
        Point_add_to_x_forward(heap, heap_counter, this, var_x);
        assert(heap[this
                    ,
                    0] == 1);
    }
    else {
        printf("Method not found");
        exit();
        assert(!(heap[this
                      ,
                      0] == 1));
    }
}
void dispatch_add_to_x_reverse(int *heap, int &heap_counter, int &this, int &var_x) {
    if (heap[this
             ,
             0] == 1) {
        Point_add_to_x_reverse(heap, heap_counter, this, var_x);
        assert(heap[this
                    ,
                    0] == 1);
    }
    else {
        printf("Method not found");
        exit();
        assert(!(heap[this
                      ,
                      0] == 1));
    }
}

void dispatch_example2_forward(int *heap, int &heap_counter, int &this) {
    if (heap[this
             ,
             0] == 2) {
        Main_example2_forward(heap, heap_counter, this);
        assert(heap[this
                    ,
                    0] == 2);
    }
    else {
        printf("Method not found");
        exit();
        assert(!(heap[this
                      ,
                      0] == 2));
    }
}
void dispatch_example2_reverse(int *heap, int &heap_counter, int &this) {
    if (heap[this
             ,
             0] == 2) {
        Main_example2_reverse(heap, heap_counter, this);
        assert(heap[this
                    ,
                    0] == 2);
    }
    else {
        printf("Method not found");
        exit();
        assert(!(heap[this
                      ,
                      0] == 2));
    }
}

void constructor_Point_forward(int *heap, int &heap_counter, int &this, int &var_x, int &var_y) {
    heap[this
         ,
         0] += 1;
    heap[this
         ,
         1] += var_x;
    heap[this
         ,
         2] += var_y;
}
void constructor_Point_reverse(int *heap, int &heap_counter, int &this, int &var_x, int &var_y) {
    heap[this
         ,
         2] -= var_y;
    heap[this
         ,
         1] -= var_x;
    heap[this
         ,
         0] -= 1;
}

void Point_add_to_x_forward(int *heap, int &heap_counter, int &this, int &var_x) {
    heap[this
         ,
         1] += var_x;
}
void Point_add_to_x_reverse(int *heap, int &heap_counter, int &this, int &var_x) {
    heap[this
         ,
         1] -= var_x;
}

void Point_add_to_y_forward(int *heap, int &heap_counter, int &this, int &var_y) {
    heap[this
         ,
         2] += var_y;
}
void Point_add_to_y_reverse(int *heap, int &heap_counter, int &this, int &var_y) {
    heap[this
         ,
         2] -= var_y;
}

void constructor_Main_forward(int *heap, int &heap_counter, int &this) {
    heap[this
         ,
         0] += 2;
}
void constructor_Main_reverse(int *heap, int &heap_counter, int &this) {
    heap[this
         ,
         0] -= 2;
}

void Main_example1_forward(int *heap, int &heap_counter, int &this) {
    int var_p = 0;
    var_p += heap_counter;
    int _parse_tmp_0 = 5;
    int _parse_tmp_1 = 8;
    constructor_Point_forward(heap, heap_counter, heap_counter, _parse_tmp_0, _parse_tmp_1);
    assert(_parse_tmp_1 == 8);
    assert(_parse_tmp_0 == 5);
    heap_counter += 1;
    int _parse_tmp_2 = 2;
    dispatch_add_to_x_forward(heap, heap_counter, var_p, _parse_tmp_2);
    assert(_parse_tmp_2 == 2);
    int _parse_tmp_3 = 3;
    dispatch_add_to_y_reverse(heap, heap_counter, var_p, _parse_tmp_3);
    assert(_parse_tmp_3 == 3);
    int _parse_tmp_4 = 7;
    int _parse_tmp_5 = 5;
    constructor_Point_reverse(heap, heap_counter, var_p, _parse_tmp_4, _parse_tmp_5);
    assert(_parse_tmp_5 == 5);
    assert(_parse_tmp_4 == 7);
    heap_counter -= 1;
    assert(var_p == heap_counter);
}
void Main_example1_reverse(int *heap, int &heap_counter, int &this) {
    int var_p = heap_counter;
    heap_counter += 1;
    int _parse_tmp_4 = 7;
    int _parse_tmp_5 = 5;
    constructor_Point_forward(heap, heap_counter, var_p, _parse_tmp_4, _parse_tmp_5);
    assert(_parse_tmp_5 == 5);
    assert(_parse_tmp_4 == 7);
    int _parse_tmp_3 = 3;
    dispatch_add_to_y_forward(heap, heap_counter, var_p, _parse_tmp_3);
    assert(_parse_tmp_3 == 3);
    int _parse_tmp_2 = 2;
    dispatch_add_to_x_reverse(heap, heap_counter, var_p, _parse_tmp_2);
    assert(_parse_tmp_2 == 2);
    heap_counter -= 1;
    int _parse_tmp_0 = 5;
    int _parse_tmp_1 = 8;
    constructor_Point_reverse(heap, heap_counter, heap_counter, _parse_tmp_0, _parse_tmp_1);
    assert(_parse_tmp_1 == 8);
    assert(_parse_tmp_0 == 5);
    var_p -= heap_counter;
    assert(var_p == 0);
}

void Main_example2_forward(int *heap, int &heap_counter, int &this) {
    int var_p = 0;
    int var_q = 0;
    var_p += heap_counter;
    int _parse_tmp_6 = 1;
    int _parse_tmp_7 = 7;
    constructor_Point_forward(heap, heap_counter, heap_counter, _parse_tmp_6, _parse_tmp_7);
    assert(_parse_tmp_7 == 7);
    assert(_parse_tmp_6 == 1);
    heap_counter += 1;
    var_q += var_p;
    int _parse_tmp_8 = 2;
    dispatch_add_to_x_forward(heap, heap_counter, var_q, _parse_tmp_8);
    assert(_parse_tmp_8 == 2);
    assert(var_q == var_p);
    int _parse_tmp_9 = 3;
    int _parse_tmp_10 = 7;
    constructor_Point_reverse(heap, heap_counter, var_p, _parse_tmp_9, _parse_tmp_10);
    assert(_parse_tmp_10 == 7);
    assert(_parse_tmp_9 == 3);
    heap_counter -= 1;
    assert(var_p == heap_counter);
}
void Main_example2_reverse(int *heap, int &heap_counter, int &this) {
    int var_p = heap_counter;
    heap_counter += 1;
    int _parse_tmp_9 = 3;
    int _parse_tmp_10 = 7;
    constructor_Point_forward(heap, heap_counter, var_p, _parse_tmp_9, _parse_tmp_10);
    assert(_parse_tmp_10 == 7);
    assert(_parse_tmp_9 == 3);
    int var_q = var_p;
    int _parse_tmp_8 = 2;
    dispatch_add_to_x_reverse(heap, heap_counter, var_q, _parse_tmp_8);
    assert(_parse_tmp_8 == 2);
    var_q -= var_p;
    heap_counter -= 1;
    int _parse_tmp_6 = 1;
    int _parse_tmp_7 = 7;
    constructor_Point_reverse(heap, heap_counter, heap_counter, _parse_tmp_6, _parse_tmp_7);
    assert(_parse_tmp_7 == 7);
    assert(_parse_tmp_6 == 1);
    var_p -= heap_counter;
    assert(var_q == 0);
    assert(var_p == 0);
}

int main() {
    int heap[10]
            [3] = {0};
    int heap_counter = 1;
    int var_m = 0;
    
    var_m += heap_counter;
    constructor_Main_forward(heap, heap_counter, heap_counter);
    heap_counter += 1;
    dispatch_example1_forward(heap, heap_counter, var_m);
    dispatch_example2_forward(heap, heap_counter, var_m);
    constructor_Main_reverse(heap, heap_counter, var_m);
    heap_counter -= 1;
    var_m -= heap_counter;
    return 1;
}
