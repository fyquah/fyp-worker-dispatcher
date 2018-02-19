OCAMLOPT=../../../../ocaml/ocamlopt

all: main.native

main.native: main.ml overrides.sexp
	$(OCAMLOPT) -inline-max-depth 5 -inlining-report \
	  -inlining-overrides overrides.sexp \
	  -O3 -S -o $@ $<

clean:
	rm main.native overrides.sexp
