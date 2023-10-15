{pkgs ? import <nixpkgs> { }}:
pkgs.stdenv.mkDerivation rec {
  name = "archive-node-database";

  buildInputs = with pkgs; [
    bash
    haskell-language-server
    rnix-lsp nixpkgs-fmt
    (postgresql.withPackages (p: [ ]))
    (haskellPackages.ghcWithPackages (self: with haskellPackages; [
      effectful curl xml tar zlib megaparsec bytestring directory tmp-postgres json process hlint optparse-applicative
    ]))
  ];

  src = ./.;

  builder = ./buildPackages.sh;
}
