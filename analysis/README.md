All the linear algebra and mathematical geneius lives in this directory.
There are a few different kinds of experiments that are carried out.

## Directory Structure

Files used to collect data from our large messy set of execution statistics:

- `extract_data_from_experiments.py` - Extract data from the archive files
  that I have collected in the 3 machines sitting in 505. The data is
  assumed to reside in /media/usb or /media/usb2 or /media/usb<n> or ...


In general, most experiments have two files:

- `learn_<exp_name>.py` - optimises things
- `print_<exp_name>.py` - print what the model has figured out, inlining
  decisions etc.

The modelling results will be storred in `modelling-results` with the
following data stucture:

```bash
out/
  almabench/
    properties.pkl
    edges_lists.pkl
    execution_times.npz
    node_labels.npz
    linear_general_reward/
      somewhat-meaningful-id/
        hyperparams.pkl
```


## Modelling Algorithms & Experiments

- `linear_specialised_reward` - the model I first talk to DT about.
  Contributions are added lienarly throughout the paths in the tree.
- `linear_general_reward` - similar to above, except using a shared
  contribution value for every experiment rather than using something that
  is specialised for every experimental run.
