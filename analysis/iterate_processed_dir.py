import csv
import os
import sys

import extract_data_from_experiments


def read_log_file(interested_exp_name, filename):
    rundirs = []
    with open(filename) as batch_f:
        for line in csv.reader(batch_f):
            (script_name, sub_rundir) = line
            exp_name = extract_data_from_experiments.script_name_to_exp_name(script_name)
            if exp_name == interested_exp_name:
                rundir = os.path.join(
                        ("/media/usb/home/fyquah/fyp/prod/processed-data/%s" % interested_exp_name),
                        os.path.basename(sub_rundir))
                yield rundir

def main():
    for rundir in read_log_file(sys.argv[1], "../important-logs/v0-data.log"):
        print rundir

if __name__ == "__main__":
    main()
