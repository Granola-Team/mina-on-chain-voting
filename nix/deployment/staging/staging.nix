let
  nixos = import <nixpkgs/nixos> {
    config = import ./configuration.nix;
  };
in
  nixos.system