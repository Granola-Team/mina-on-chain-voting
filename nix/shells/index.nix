{ pkgs, apps }:
let
  rust = pkgs.rust-bin.stable.latest.default.override {
    extensions = [ "rust-src" ];
  };

  clientDependencies = with pkgs; [
    yarn rnix-lsp nixpkgs-fmt
  ];

  serverDependencies = with pkgs; [
    rust rust-analyzer rustfmt
    rnix-lsp nixpkgs-fmt
    pkg-config openssl
    postgresql
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
        haskell-language-server
        rnix-lsp nixpkgs-fmt
        geos gdal
        (postgresql.withPackages (p: [ p.postgis ]))
        (haskellPackages.ghcWithPackages (self: with haskellPackages; [
          effectful curl xml tar zlib megaparsec bytestring directory tmp-postgres json process hlint
        ]))

        apps.clean-archive-backups
        apps.download-archive-dump
        apps.run-temp-database
        apps.run-end-to-end

        morph
      ];

      shellHook = ''
        runghc Tools/downloadArchiveDump.hs
      '';
    };
  };
}