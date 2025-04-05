FROM ocaml/opam:debian

COPY bin/* bin/
COPY lib/* lib/
COPY weaken.opam dune-project ./

RUN sudo apt-get -y install pkg-config libgmp-dev zlib1g-dev
RUN opam install -y --deps-only .
## Detect Alt-Ergo prover
RUN opam exec -- why3 config detect

ENTRYPOINT [ "opam", "exec", "--", "dune", "exec", "weaken" ]
