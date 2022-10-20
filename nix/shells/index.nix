{ pkgs, apps }:
let
  rust = pkgs.rust-bin.stable.latest.default.override {
    extensions = [ "rust-src" ];
  };

  clientDependencies = with pkgs; [
    yarn rnix-lsp nixpkgs-fmt
  ];

  serverDependencies = with pkgs; [
    rust rust-analyzer rustfmt sqlx-cli 
    cargo-kcov
    rnix-lsp nixpkgs-fmt
    pkg-config openssl
    haskellPackages.dotenv
    postgresql sqlite
  ];
in {
  devShells = {
    client = import ./client.nix {
      inherit pkgs clientDependencies;
    };

    server = import ./server.nix {
      inherit pkgs serverDependencies;
    };

    default = pkgs.mkShell {

      buildInputs = with pkgs; [
        rust rust-analyzer rustfmt sqlx-cli cargo-kcov
        rnix-lsp nixpkgs-fmt
        pkg-config openssl
        haskellPackages.dotenv
        postgresql sqlite
        morph
      ];

      shellHook = ''
      '';
    };
  };
}