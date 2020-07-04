//
// Created by kiva on 2020/7/4.
//
#pragma once

#include <stdlib.h>
#include <string.h>

#define MOVE(v) v

#define NEW(type) ((String *) __mem_alloc(sizeof(type)))
#define NEW_SIZED(size) (__mem_alloc(size))

#define DELETE(ptr) __mem_dealloc(ptr)

static inline void *__mem_alloc(size_t size) {
    void *ptr = malloc(size);
    if (ptr) {
        memset(ptr, '\0', size);
    }
    return ptr;
}

static inline void __mem_dealloc(void *ptr) {
    free(ptr);
}
