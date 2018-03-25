.PHONY: build image
ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))  # https://stackoverflow.com/a/23324703/547223
rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))  # https://stackoverflow.com/a/12959764/547223

build: $(call rwildcard,src/,*.rs) Cargo.toml
	mkdir -p build-cache/registry
	mkdir -p build-cache/target
	docker run --rm -v ${ROOT_DIR}/build-cache/registry:/home/rust/.cargo/registry:rw -v ${ROOT_DIR}:/home/rust/src:ro -v ${ROOT_DIR}/build-cache/target:/home/rust/src/target:rw ekidd/rust-musl-builder:stable cargo build --release

image: build
	docker build -t kgadek/gumby .