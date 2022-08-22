{
  description = "OnChainSignalling Client";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, flake-compat }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ocs-client = import ./ocs-client.nix { 
          inherit nixpkgs system; 
        };
      in
      rec {
        defaultPackage = ocs-client.defaultPackage;
        devShell = ocs-client.devShell;
      }
    );
}