{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/22.05";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, flake-compat }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      rec {
        defaultPackage = pkgs.mkYarnPackage {
          name = "ocs-client";
          version = "0.0.1";
          src = ./.;
          buildPhase = ''
            yarn run build --offline
            mkdir -p $out/out
          '';
          distPhase = "true";
          installPhase = ''
            cp -r deps/ocs-client/build/* $out/out/
          '';
        };
      }
    );
}
