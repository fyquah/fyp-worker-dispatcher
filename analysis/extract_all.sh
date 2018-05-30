#!/bin/bash

set -euo pipefail

echo "VERSION = $VERSION"

echo "Running extract_all_commands in parallel!"
parallel $PARALLEL_FLAGS --verbose <extract_all_commands.txt
echo "Done!!"
