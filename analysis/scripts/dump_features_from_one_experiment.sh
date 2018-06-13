set -euo pipefail
exp="$1"
find /media/usb/home/fyquah/fyp/prod/processed-data/$exp/ -name 'queries-v0.bin' | ../_build/default/tools/data_cleaner.exe concat-queries -output ../w/$exp/queries-v0.bin -filelist /dev/stdin -tag $exp
