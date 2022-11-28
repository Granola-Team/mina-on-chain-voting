{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
    buildInputs = with pkgs; [
        rustup rust-analyzer rustfmt cargo-kcov
        git pkg-config
        openssl postgresql sqlite
    ];
}