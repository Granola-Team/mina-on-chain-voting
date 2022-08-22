{ nixpkgs ? <nixpkgs> }:
let
  rust-overlay = builtins.fetchTarball "https://github.com/oxalica/rust-overlay/archive/master.tar.gz";
  overlays = [ (import rust-overlay) ];
  pkgs = import nixpkgs { inherit overlays;  };
  ocs-server = import ./ocs-server.nix { inherit nixpkgs rust-overlay; system = builtins.currentSystem; };
in ocs-server.devShell