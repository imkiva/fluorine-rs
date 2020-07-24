//
// Created by kiva on 2020/7/4.
//
#pragma once

#include <stdbool.h>
#include "string.h"
#include "vec.h"

typedef double f64;
struct FLValue;

typedef struct {
    String *name;
    size_t fields;
} FLEnumVariant;

typedef struct {
    String *name;
    Vec(FLEnumVariant) *variants;
} FLEnumType;

typedef struct {
    size_t argc;
    Vec(FLValue) *argv;
    struct FLValue *(*fn)(Vec(FLValue) *argv);
} FLLambda;

typedef struct {
    FLEnumType type;
    FLEnumVariant *variant;
    Vec(FLValue) *fields;
} FLEnum;

typedef enum {
    UnitType,
    NumberType,
    BoolType,
    StringType,
    LambdaType,
    EnumType,
} FLTypeTag;

typedef struct FLValue {
    FLTypeTag tag;
    union {
        f64 number;
        bool boolean;
        String *string;
        FLLambda *lambda;
        FLEnum *enu;
    };
} FLValue;
