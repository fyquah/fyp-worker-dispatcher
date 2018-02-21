# Remove duplicates and maintains order.

import sys

s = set()
lines = []
filename = sys.argv[1]

with open(filename, "r") as f:
    for line in f:
        if line not in s:
            s.add(line)
            lines.append(line)


with open(filename, "w") as f:
    for line in lines:
        f.write(line)
