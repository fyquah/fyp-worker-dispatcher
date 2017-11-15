PATH_TO_BINARY="$1"
ARGS="$2"

OCAML_GC_STATS=/dev/stderr ./main.native $ARGS 2>&1 1>/dev/null
