{ pkgs, defaultSource }:
let
  rust = pkgs.rust-bin.stable.latest.default.override {
    extensions = [ "rust-src" ];
  };

  clientSource = "${defaultSource}/client";
  clientDependencies = with pkgs; [
    yarn
  ];

  serverSource = "${defaultSource}/server";
  serverDependencies = with pkgs; [
    rust
    pkg-config openssl
    postgresql
  ];
in {
  packages = rec {
    client = import ./client.nix { 
      inherit pkgs clientSource clientDependencies;
    };

    server = import ./server.nix { 
      inherit pkgs serverSource serverDependencies;
      rustPlatform = pkgs.makeRustPlatform {
        rustc = rust;
        cargo = rust;
      };
    };

    default = pkgs.stdenv.mkDerivation {
      name = "On Chain Signalling";
      version = "1.0.0";
      src = defaultSource;

      buildPhase = ''
        mkdir $out
        mkdir $out/bin
        mkdir $out/out
      '';

      installPhase = ''
        cp ${server}/bin/ocs_api $out/bin/ocs_api
        cp -r ${client}/out/* $out/out/
      '';
    };
  };
}