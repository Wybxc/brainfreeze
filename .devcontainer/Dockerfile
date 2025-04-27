FROM coqorg/coq:8.20.1-ocaml-4.14.2-flambda

RUN opam update && \
    opam install -y vscoq-language-server ocaml-lsp-server ocamlformat core odig pprint
