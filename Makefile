.PHONY: dev release static image deps test clean run install

dev:     ; opam exec -- dune build
release: ; opam exec -- dune build --profile release --build-dir _build_release

image:; docker build --tag ffbar:latest .
static:
	docker run --rm --volume=$(shell pwd):/build ffbar \
		opam exec -- dune build --profile static --build-dir _build_static && \
		strip _build_static/default/bin/main.exe

deps:  ; opam install --deps-only --yes .
test:  ; opam exec -- dune runtest
clean: ; opam exec -- dune clean ; rm -rf _build_release _build
run:   ; opam exec -- dune exec ffbar

install: release
	install -s _build_release/default/bin/main.exe $(HOME)/bin/ffbar
