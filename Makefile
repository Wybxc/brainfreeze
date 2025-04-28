all: coq ml

coq: extracted/CoqExtract.ml extracted/CoqExtract.mli

ml: _build/default/main.exe

.PHONY: all coq ml clean

CoqMakefile CoqMakefile.conf: _CoqProject src/*.v
	coq_makefile -f _CoqProject -o CoqMakefile

extracted/CoqExtract.ml extracted/CoqExtract.mli: CoqMakefile CoqMakefile.conf
	make -f CoqMakefile

_build/default/main.exe: extracted/CoqExtract.ml extracted/CoqExtract.mli
	dune build -j 8

clean:
	rm -f CoqMakefile CoqMakefile.conf .CoqMakefile.d
	rm -f extracted/CoqExtract.ml extracted/CoqExtract.mli
	rm -rf _build
