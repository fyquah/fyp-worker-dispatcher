all: main.native

main.native: main.ml overrides.sexp
	ocamlopt -inline-max-depth 5 -inlining-report \
	  -inlining-overrides overrides.sexp \
	  -O3 -S -o $@ $<
