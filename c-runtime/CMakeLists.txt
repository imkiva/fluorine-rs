cmake_minimum_required(VERSION 3.10)
project(fluorine-rs C)

set(CMAKE_C_STANDARD 99)

add_library(cfluorine STATIC value.h value.c string.c string.h mem.h vec.c vec.h)
