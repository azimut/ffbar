FROM alpine:3.19

RUN apk add \
    bash\
    bubblewrap\
    coreutils\
    gcc\
    git\
    m4\
    make\
    musl-dev\
    opam

RUN opam init\
    --disable-sandboxing\
    --auto-setup\
    --compiler ocaml-base-compiler.5.1.1

WORKDIR /build

ADD Makefile /build/
ADD ffbar.opam /build/

RUN make deps
