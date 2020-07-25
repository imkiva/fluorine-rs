//
// Created by kiva on 2020/7/6.
//
#pragma once

#include <stdbool.h>

#include "mem.h"

#define Vec(type) Vec

typedef struct {
    size_t cap;
    size_t len;
    void **data;
} Vec;

extern Vec *Vec_new();
extern Vec *Vec_with_capacity(size_t cap);
extern void Vec_delete(Vec *MOVE(this));

extern bool Vec_push(Vec *this, void *MOVE(element));
extern void *Vec_pop(Vec *this);
extern void *Vec_at(Vec *this, size_t index);

