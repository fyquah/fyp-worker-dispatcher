#!/bin/bash

set -euo pipefail

echo "VERSION = $VERSION"

echo "Running extract_all_commands in parallel!"
cat extract_all_commands.txt | sed -e "s/VERSION/$VERSION/g" | parallel -j 7 --verbose 
echo "Done!!"
