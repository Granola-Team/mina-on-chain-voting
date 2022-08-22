{
  description = "API to poll a Mina Archive Node to accrue votes for the Mina Magenta hard fork";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/22.05";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils, flake-compat }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ocs-server = import ./ocs-server.nix { 
          inherit nixpkgs system rust-overlay; 
        };

      in rec {
        defaultPackage = ocs-server.defaultPackage;
        devShell = ocs-server.devShell;
      }
    );
}