{
  description = "On Chain Signalling Deployment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    deploy-rs-flake.url = "github:serokell/deploy-rs";
  };

  outputs = { self, nixpkgs, flake-utils, flake-compat, deploy-rs-flake, rust-overlay }: 
    (import ./nix/index.nix { 
      inherit self nixpkgs flake-utils flake-compat deploy-rs-flake rust-overlay; 
      sourceDir = ./.;
    });
}