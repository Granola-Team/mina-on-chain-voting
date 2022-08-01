{
  description = "On Chain Signalling Deployment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    deploy-rs.url = "github:serokell/deploy-rs";
    mina.url = "github:MinaProtocol/mina";

    ocs-server.url = "path:server";
    ocs-server.inputs.nixpkgs.follows = "nixpkgs";

    ocs-client.url = "path:client";
    ocs-client.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, flake-compat, deploy-rs, mina, ocs-server, ocs-client }: 
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        server = ocs-server.defaultPackage;
        client = ocs-client.defaultPackage;

        appDependencies = with pkgs; [
          geos gdal
          # postgres with postgis support
          (postgresql.withPackages (p: [ p.postgis ]))

          (haskellPackages.ghcWithPackages (self: with haskellPackages; [
            curl xml tar zlib fused-effects megaparsec bytestring directory tmp-postgres json process
          ]))
        ];
      in rec {

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
              ocs-server.packages.${system}.onChainSignalling-api
              ocs-client.defaultPackage.${system}
            ];
            text = ''
              CLIENT_BUILD_DIR=${ocs-client.defaultPackage.${system}}/out on_chain_signalling-api
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
          version = "0.1.0";
          src = ./.;

          buildPhase = ''
            mkdir $out
            mkdir $out/bin
            mkdir $out/out
          '';

          installPhase = ''
           cp ${ocs-server.packages.${system}.onChainSignalling-api}/bin/on_chain_signalling-api $out/bin/ocs-api
           cp -r ${ocs-client.defaultPackage.${system}}/out/* $out/out/
          '';
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
