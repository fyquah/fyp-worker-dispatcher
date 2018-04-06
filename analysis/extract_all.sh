#!/bin/bash

echo "Running extract_all_commands in parallel!"
parallel $PARALLEL_FLAGS <extract_all_commands.txt
echo "Done!!"
