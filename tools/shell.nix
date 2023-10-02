{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    bash
    haskell-language-server
    rnix-lsp nixpkgs-fmt
    geos gdal
    (postgresql.withPackages (p: [ p.postgis ]))
    (haskellPackages.ghcWithPackages (self: with haskellPackages; [
      effectful curl xml tar zlib megaparsec bytestring directory tmp-postgres json process hlint
    ]))
  ];

  shellHook = ''
  '';
}