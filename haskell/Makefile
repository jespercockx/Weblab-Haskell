IMAGE_NAME := weblab/cse3100-2020-haskell
DOCKERFILE_PATH := Dockerfile

MAKEFILE := $(abspath $(lastword $(MAKEFILE_LIST)))
SELF_DIR := $(dir $(MAKEFILE))
CMD := /bin/bash
BUILD_PATH := $(SELF_DIR)

all: build

include $(SELF_DIR)../Makefile.inc
