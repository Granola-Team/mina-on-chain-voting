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
  };

  outputs = { self, nixpkgs, flake-utils, flake-compat, rust-overlay }: 
    (import ./nix/index.nix { 
      inherit self nixpkgs flake-utils flake-compat rust-overlay; 
      sourceDir = ./.;
    });
}