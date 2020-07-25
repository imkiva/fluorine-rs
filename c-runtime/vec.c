//
// Created by kiva on 2020/7/6.
//

#include "vec.h"

Vec *Vec_new() {
    return NEW(Vec);
}

Vec *Vec_with_capacity(size_t cap) {
    Vec *vec = Vec_new();
    if (cap > 0) {
        vec->cap = cap;
        vec->data = NEW_SIZED(sizeof(void *) * cap);
    }
    return vec;
}

void Vec_delete(Vec *MOVE(this)) {
    DELETE(this->data);
    DELETE(this);
}

bool Vec_push(Vec *this, void *MOVE(element)) {
    if (this->len < this->cap) {
        this->data[this->len++] = element;
        return true;
    }

    size_t new_cap = (this->cap + 1) * 1.75;
    void *new_data = realloc(this->data, sizeof(void *) * new_cap);
    if (new_data) {
        this->cap = new_cap;
        this->data = new_data;
        this->data[this->len++] = element;
        return true;
    } else {
        return false;
    }
}

void *Vec_pop(Vec *this) {
    return this->len == 0 ? NULL : this->data[this->len--];
}

void *Vec_at(Vec *this, size_t index) {
    return index >= this->len ? NULL : this->data[index];
}

