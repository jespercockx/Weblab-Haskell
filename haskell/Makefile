IMAGE_NAME := weblab/haskell
DOCKERFILE_PATH := Dockerfile

MAKEFILE := $(abspath $(lastword $(MAKEFILE_LIST)))
SELF_DIR := $(dir $(MAKEFILE))
CMD := /bin/bash
BUILD_PATH := $(SELF_DIR)

all: build

include $(SELF_DIR)../Makefile.inc
