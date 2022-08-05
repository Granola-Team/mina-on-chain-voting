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
        pkgs = import nixpkgs { inherit system; };
      in
      rec {
        defaultPackage = pkgs.mkYarnPackage {
          name = "ocs-client";
          version = "1.0.0";
          src = ./.;
          buildPhase = ''
            yarn build
            mkdir -p $out/out
          '';
          distPhase = "true";
          installPhase = ''
            cp -r deps/ocs-client/build/* $out/out/
          '';
        };

        devShell = pkgs.mkShell {

          buildInputs = with pkgs; [
            yarn rnix-lsp nixpkgs-fmt
          ];

          shellHook = ''
            yarn install && yarn upgrade
          '';
        };
      }
    );
}