//
// Created by kiva on 2020/7/4.
//
#pragma once

#include <stddef.h>
#include <stdbool.h>

#include "men.h"

typedef struct {
    size_t len;
    char data[0];
} String;

extern String *String_new();
extern String *String_with_capacity(size_t cap);
extern String *String_from(const char *str);
extern void String_delete(String *MOVE(this));
extern const char *String_as_str(String *this);

extern String *String_concat(String *MOVE(lhs), String *MOVE(rhs));
extern bool String_starts_with(String *this, String *rhs);
extern bool String_starts_with2(String *this, const char *rhs);

extern ssize_t String_index_of(String *this, String *rhs);
extern ssize_t String_index_of2(String *this, const char *rhs);
extern ssize_t String_index_of_char(String *this, char ch);

extern bool String_eq(String *lhs, String *rhs);
extern bool String_nq(String *lhs, String *rhs);
extern bool String_lt(String *lhs, String *rhs);
extern bool String_le(String *lhs, String *rhs);
extern bool String_gt(String *lhs, String *rhs);
extern bool String_ge(String *lhs, String *rhs);
