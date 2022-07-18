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
      url = "github:devbaze/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils, flake-compat }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];

        pkgs = import nixpkgs { inherit system overlays; };

        rust = pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-src" ];
        };

        rustPlatform = pkgs.makeRustPlatform {
          rustc = rust;
          cargo = rust;
        };

        dependencies = with pkgs; [
          rust rust-analyzer rustfmt
          rnix-lsp nixpkgs-fmt
          pkg-config openssl
          postgresql
        ];

      in rec {
        packages = flake-utils.lib.flattenTree {
          onChainSignalling-api = rustPlatform.buildRustPackage rec {
            pname = "onChainSignalling-api";
            version = "0.0.1";
            nativeBuildInputs = dependencies;

            src = ./.;

            cargoLock = { lockFile = ./Cargo.lock; };

            verifyCargoDeps = true;
          };
        };

        defaultPackage = packages.onChainSignalling-api;

        devShell = pkgs.mkShell {
          packages = dependencies;
          shellHook = ''
          '';
        };
      }
    );
}