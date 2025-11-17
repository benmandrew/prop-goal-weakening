FROM ocaml/opam:debian-12-ocaml-5.2@sha256:de66791dbf26b74497db7a6d86ddd7d9af21deb2d5d624bd3a893bfa2421b1d9

RUN sudo apt-get -y install pkg-config libgmp-dev zlib1g-dev

COPY . ./
RUN opam install -y --deps-only .
## Detect Alt-Ergo prover
RUN opam exec -- why3 config detect

RUN opam exec -- dune build @install

ENTRYPOINT [ "_build/install/default/bin/weaken" ]
