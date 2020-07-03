cmake_minimum_required(VERSION 3.16)
project(c_runtime C)

set(CMAKE_C_STANDARD 99)

add_library(cfluorine STATIC value.h value.c string.c string.h move.h)
