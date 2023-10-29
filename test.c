typedef struct {
    void* clfunc
    void* capture_2;
} clstruct_true_5;

void* clfunc_true_6(clstruct_true_5* self, void* param) {
    return self->capture_2;
}

typedef struct {
    void* clfunc
    void* capture_1;
} clstruct_true_2;

void* clfunc_true_3(clstruct_true_2* self, void* param) {
    clstruct_true_5* c4;
    c4->capture_2 = self->capture_1
    c4->clfunc = clfunc_true_6
    return c4;
}

void* true_func(void) {
    clstruct_true_2* c1;
    c1->capture_1 = param
    c1->clfunc = clfunc_true_3
    return c1;
}

typedef struct {
    void* clfunc
    void* capture_1;
} clstruct_false_5;

void* clfunc_false_6(clstruct_false_5* self, void* param) {
    return param;
}

typedef struct {
    void* clfunc
} clstruct_false_2;

void* clfunc_false_3(clstruct_false_2* self, void* param) {
    clstruct_false_5* c4;
    c4->capture_1 = param
    c4->clfunc = clfunc_false_6
    return c4;
}

void* false_func(void) {
    clstruct_false_2* c1;
    c1->clfunc = clfunc_false_3
    return c1;
}


