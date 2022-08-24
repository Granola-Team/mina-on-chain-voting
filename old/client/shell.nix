{ nixpkgs ? <nixpkgs> }:

let
  ocs-client = import ./ocs-client.nix { inherit nixpkgs; system = builtins.currentSystem; };
in ocs-client.devShell