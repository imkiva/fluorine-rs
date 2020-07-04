//
// Created by kiva on 2020/7/4.
//

#include "string.h"

String *String_new() {
    return NEW_SIZED(sizeof(String) + 1);
}

String *String_with_capacity(size_t cap) {
    String *s = NEW_SIZED(sizeof(String) + cap + 1);
    s->len = cap;
    return s;
}

String *String_from(const char *str) {
    if (!str) {
        return String_new();
    }

    size_t len = strlen(str);
    String *s = String_with_capacity(len);
    memcpy(s->data, str, len);
    return s;
}

void String_delete(String *MOVE(this)) {
    DELETE(this);
}

const char *String_as_str(String *this) {
    return this->data;
}

String *String_concat(String *MOVE(lhs), String *MOVE(rhs)) {
    String *s = String_with_capacity(lhs->len + rhs->len);
    strcat(s->data, lhs->data);
    strcat(s->data, rhs->data);
    return s;
}

bool String_starts_with(String *this, String *rhs) {
    return strncmp(this->data, rhs->data, rhs->len);
}

bool String_starts_with2(String *this, const char *rhs) {
    return strncmp(this->data, rhs, strlen(rhs)) == 0;
}

ssize_t String_index_of(String *this, String *rhs) {
    return String_index_of2(this, rhs->data);
}

ssize_t String_index_of2(String *this, const char *rhs) {
    char *p = strstr(this->data, rhs);
    return p ? p - this->data : -1;
}

ssize_t String_index_of_char(String *this, char ch) {
    char *p = strchr(this->data, ch);
    return p ? p - this->data : -1;
}

bool String_eq(String *lhs, String *rhs) {
    return strcmp(lhs->data, rhs->data) == 0;
}

bool String_nq(String *lhs, String *rhs) {
    return strcmp(lhs->data, rhs->data) != 0;
}

bool String_lt(String *lhs, String *rhs) {
    return strcmp(lhs->data, rhs->data) < 0;
}

bool String_le(String *lhs, String *rhs) {
    return strcmp(lhs->data, rhs->data) <= 0;
}

bool String_gt(String *lhs, String *rhs) {
    return strcmp(lhs->data, rhs->data) > 0;
}

bool String_ge(String *lhs, String *rhs) {
    return strcmp(lhs->data, rhs->data) >= 0;
}

