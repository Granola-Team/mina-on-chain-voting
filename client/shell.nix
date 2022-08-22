{ nixpkgs ? <nixpkgs> }:

let
  ocs-client = import ./ocs-client.nix { inherit nixpkgs; };
in ocs-client.devShell