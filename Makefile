.PHONY: dev release static docker image deps test clean run install

dev:     ; opam exec -- dune build
release: ; opam exec -- dune build --profile release --build-dir _build_release
static:
	opam exec -- dune build --profile static --build-dir _build_static
	strip _build_static/default/bin/main.exe

image:; docker build --tag ffbar:latest .
docker:
	docker run --rm --volume=$(shell pwd):/build ffbar make static

deps:  ; opam install --deps-only --yes .
test:  ; opam exec -- dune runtest
clean: ; opam exec -- dune clean ; rm -rf _build_release _build
run:   ; opam exec -- dune exec ffbar

install: release
	install -s _build_release/default/bin/main.exe $(HOME)/bin/ffbar
