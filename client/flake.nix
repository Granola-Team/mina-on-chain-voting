{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/22.05";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, flake-compat }: 
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        version = "0.0.1";
        # yarnDeps = pkgs.stdenv.mkYarnModules {
        #   pname = "ocs-client-yarn-deps";
        #   inherit version;
        #   packageJSON = ./package.json;
        #   yarnLock = ./yarn.lock;
        #   yarnNix = ./yarn.nix;
        #   preBuild = ''
        #   '';
        #   postBuild = ''
        #   '';
        # };
      in rec {
        defaultPackage = pkgs.mkYarnPackage {
          name = "ocs-client";
          inherit version;
          src = ./.;
          buildPhase = ''
            yarn run build --offline
            mkdir -p $out/out
          '';
          distPhase = "true";
          installPhase = ''
            cp -r deps/ocs-client/build/* $out/out/
          '';
        };
      }
    );
}
