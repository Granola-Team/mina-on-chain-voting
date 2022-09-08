#!/usr/bin/env nix-shell
# Mara\ The above shebang line will use `nix-shell`
# to create the environment of this shell script.

# Mara\ Specify the packages we are using in this
# script as well as the fact that we are running it
# in bash.
#! nix-shell -p morph -i bash

# Mara\ Explode on any error.
set -e

# Mara\ Build the system configurations for every
# machine in this network and register them as
# garbage collector roots so `nix-collect-garbage`
# doesn't sweep them away.
morph build --keep-result ./network.nix

# Mara\ Push the config to the hosts.
morph push ./network.nix

# Mara\ Activate the NixOS configuration on the
# network.
morph deploy ./network.nix switch