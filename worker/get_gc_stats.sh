PATH_TO_BINARY="$1"
shift
ARGS="$@"

OCAML_GC_STATS=/dev/stderr "$PATH_TO_BINARY" $ARGS 2>&1 1>/dev/null
