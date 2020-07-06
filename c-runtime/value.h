//
// Created by kiva on 2020/7/4.
//
#pragma once

#include <stdbool.h>
#include "string.h"

typedef double f64;

typedef enum {
    UnitType,
    NumberType,
    BoolType,
    StringType,
    LambdaType,
    EnumType,
} FLTypeTag;

typedef struct {
    FLTypeTag tag;
    union {
        f64 number;
        bool boolean;
        String *string;
        void *lambda;
        void *enu;
    };
} FLValue;
