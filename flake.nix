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
    deploy-rs.url = "github:serokell/deploy-rs";
    mina.url = "github:MinaProtocol/mina";
  };

  outputs = { self, nixpkgs, flake-utils, flake-compat, deploy-rs, mina, rust-overlay }: 
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        appDependencies = with pkgs; [
          geos gdal
          # postgres with postgis support
          (postgresql.withPackages (p: [ p.postgis ]))

          (haskellPackages.ghcWithPackages (self: with haskellPackages; [
            curl xml tar zlib fused-effects megaparsec bytestring directory tmp-postgres json process
          ]))
        ];

        ocs-client = import ./client/ocs-client.nix { 
          inherit nixpkgs; 
        };
        ocs-server = import ./server/ocs-server.nix { 
          inherit nixpkgs rust-overlay; 
        };

      in rec {

        packages = flake-utils.lib.flattenTree {
          ocs-client = ocs-client.defaultPackage;
          ocs-server = ocs-server.defaultPackage;
        };

        apps = flake-utils.lib.flattenTree {
          clean-archive-backups = pkgs.writeShellApplication {
            name = "clean-archive-backups";
            runtimeInputs = appDependencies;
            text = "runghc ./Tools/cleanArchiveDumps.hs";
          };

          download-archive-dump = pkgs.writeShellApplication {
            name = "download-archive-dump";
            runtimeInputs = appDependencies;
            text = "runghc ./Tools/downloadArchiveDump.hs";
          };

          run-temp-database = pkgs.writeShellApplication {
            name = "run-temp-database";
            runtimeInputs = appDependencies;
            text = "runghc ./Tools/runTempDatabase.hs";
          };

          run-end-to-end = pkgs.writeShellApplication {
            name = "run-end-to-end";
            runtimeInputs = [
              packages.ocs-api
              packages.ocs-client
            ];
            text = ''
              CLIENT_BUILD_DIR=${packages.ocs-client}/out ocs_api
            '';
          };
        };

        defaultApp = apps.run-end-to-end;

        deploy.nodes.staging = {
          hostname = "35.203.38.140";
          user = "robinbateboerop";

          profiles = { 
            system = {
              user = "root";
              path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.staging;
            };

            onchain-signalling = {
              path = deploy-rs.lib.x86_64-linux.activate.custom apps.run-end-to-end "./bin/run-end-to-end";
            };
          };
        };

        checks = builtins.mapAttrs (system: 
          deployLib: deployLib.deployChecks self.deploy
        ) deploy-rs.lib;

        defaultPackage = pkgs.stdenv.mkDerivation {
          name = "On Chain Signalling";
          version = "1.0.0";
          src = ./.;

          buildPhase = ''
            mkdir $out
            mkdir $out/bin
            mkdir $out/out
          '';

          installPhase = ''
           cp ${packages.ocs-server}/bin/ocs_api $out/bin/ocs_api
           cp -r ${packages.ocs-client}/out/* $out/out/
          '';
        };

        devShells = flake-utils.lib.flattenTree {
          ocs-client = ocs-client.devShell;
          ocs-server = ocs-server.devShell;
        };

        devShell = pkgs.mkShell {

          buildInputs = with pkgs; [
            haskell-language-server
            rnix-lsp nixpkgs-fmt
            geos
            gdal
            nixpkgs-fmt
            # postgres with postgis support
            (postgresql.withPackages (p: [ p.postgis ]))

            (haskellPackages.ghcWithPackages (self: with haskellPackages; [
              effectful curl xml tar zlib megaparsec bytestring directory tmp-postgres json process
            ]))

            apps.clean-archive-backups
            apps.download-archive-dump
            apps.run-temp-database
            apps.run-end-to-end
          ];

          shellHook = ''
            runghc Tools/downloadArchiveDump.hs
          '';
        };
      }
    );
}