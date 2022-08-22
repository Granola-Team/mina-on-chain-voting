{ nixpkgs, rust-overlay }:

let
  overlays = [ (import rust-overlay) ];
  pkgs = import nixpkgs { inherit overlays; };

  rust = pkgs.rust-bin.stable.latest.default.override {
    extensions = [ "rust-src" ];
  };

  rustPlatform = pkgs.makeRustPlatform {
    rustc = rust;
    cargo = rust;
  };

  serverDependencies = with pkgs; [
    rust rust-analyzer rustfmt
    rnix-lsp nixpkgs-fmt
    pkg-config openssl
    postgresql
  ];
in {
  defaultPackage = rustPlatform.buildRustPackage rec {
    pname = "ocs_api";
    version = "1.0.0";
    nativeBuildInputs = serverDependencies;

    src = ./.;

    cargoLock = { lockFile = ./Cargo.lock; };

    verifyCargoDeps = true;
  };

  devShell = pkgs.mkShell {
    buildInputs = serverDependencies;
    shellHook = ''
      cd server
      cargo check && cargo test
    '';
  };
}