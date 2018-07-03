# Setting up a Master Node

*Ideally, this should be a vagrantfile, but I did not have time to do this.*

I have tested these instructions in a vagrant instance. The reason why this
isn't a vagrant instance is because I was struggling to get a vagrant debian
box with the right disk sizes.

Install debian dependencies

```bash
# Packages for building things
sudo apt-get install zip build-essential git tmux curl ocaml # (Yes, we need a system copy of OCaml)

# These are required by owl
sudo apt-get install libgsl0-dev liblapacke-dev libopenblas-dev pkg-config libplplot-dev libshp-dev m4 libffi-dev python-pip
```

Generate SSH keys

```bash
ssh-keygen
```

Clone all the relevant repos:

```bash
mkdir -p ~/fyp
git clone https://github.com/fyquah95/fyp-worker-dispatcher.git worker-dispatcher
git clone https://github.com/fyquah95/fyp-ocaml.git ocaml
git clone https://github.com/fyquah95/fyp-experiments.git experiments
git clone https://github.com/fyquah95/fyp-opam-repo-dev.git opam-repo-dev
git clone https://github.com/fyquah95/tensorflow-ocaml.git tensorflow-ocaml
```

Install opam:

```bash
wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sudo sh -s /usr/local/bin
```

Compile OCaml compiler for the master node (ocaml 4.06.1):

```bash
opam init
opam switch 4.06.1
eval `opam config env`
```

Change the line with `/home/fyquah` in
~/fyp/opam-repo-dev/compilers/4.06.0+fyp.comp to the relevant home directory
in your setup.

Compile OCaml compiler for compiling benchmarks (with support for inlining
policies and overrides):

```bash
cd ~/fyp/worker-dispatcher
./scripts/configure-fyp-env
```

Install all dependencies required by the packages for tools, data generation
(This is for the main compiler)

```bash
pip install tensorflow
opam pin add tensorflow ~/fyp/tensorflow-ocaml # Then, click no
cd ~/fyp/worker-dispatcher/metadata-filelist
opam switch import universe_for_4.06.1
```

Compile everything!

```bash
make ocamlopt
```

Install `ocamlnow`

```bash
echo 'export PATH="$PATH:$HOME/bin/"' >> ~/.bashrc
mkdir -p ~/bin/
cp _build/default/tools/now.exe ~/bin/ocamlnow
source ~/.bashrc
```

Install python dependencies. I assume you have python2.7

```bash
cd ~/fyp/worker-dispatcher/metadata-filelist
pip install <python-requirements.txt
```
