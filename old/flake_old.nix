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
    deploy-rs-flake.url = "github:serokell/deploy-rs";
  };

  outputs = { self, nixpkgs, flake-utils, flake-compat, deploy-rs-flake, rust-overlay }: 
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
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
          inherit nixpkgs system;
        };
        ocs-server = import ./server/ocs-server.nix { 
          inherit nixpkgs system rust-overlay; 
        };

        deploy-rs = deploy-rs-flake.defaultPackage.${system};

      in rec {

        ### BUILD CONFIGURATION ###

        packages.ocs-client = ocs-client.defaultPackage; #ocs-client
        packages.ocs-server = ocs-server.defaultPackage; # nix build .#ocs-server

        # nix build
        packages.default = pkgs.stdenv.mkDerivation {
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

        ### APPLICATIONS USING FLAKE BINARIES ###

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

          run-archive-node = pkgs.writeShellApplication {
            name = "run-archive-node";
            runtimeInputs = [];
            text = ''
            '';
          };

          run-archive-database = pkgs.writeShellApplication {
            name = "run-archive-database";
            runtimeInputs = appDependencies ++ [apps.download-archive-dump];
            text = let
              postgresConf =
                pkgs.writeText "postgresql.conf"
                  ''
                    # Add Custom Settings
                    log_min_messages = warning
                    log_min_error_statement = error
                    log_min_duration_statement = 100  # ms
                    log_connections = on
                    log_disconnections = on
                    log_duration = on
                    #log_line_prefix = '[] '
                    log_timezone = 'UTC'
                    log_statement = 'all'
                    log_directory = 'pg_log'
                    log_filename = 'postgresql-%Y-%m-%d_%H%M%S.log'
                    logging_collector = on
                    log_min_error_statement = error
                  '';
            in ''
              echo "Using ${pkgs.postgresql.name}."

              # Setup: other env variables
              export PGHOST="$PGDATA"
              # Setup: DB
              [ ! -d "$PGDATA" ] && pg_ctl initdb -o "-U postgres" && cat "${postgresConf}" >> "$PGDATA"/postgresql.conf
            # shellcheck disable=SC2154,SC2139
              pg_ctl -o "-p '$archive-node-database-port' -k \"$PGDATA\"" start
              # alias fin="pg_ctl stop && exit"
              # alias pg="psql -p $archive-node-database-port -U postgres"

              download-archive-dump
              
            '';
          };

          run-end-to-end = pkgs.writeShellApplication {
            name = "run-end-to-end";
            runtimeInputs = [
              packages.ocs-server
              packages.ocs-client
            ];
            text = ''
            # shellcheck disable=SC2154,SC2139
              PORT="onchain-signalling-port" \
              DBPORT="$archive-node-database-port" \
              PASSWD=postgres \
              USER=postgres \
              DBNAME=archive_balances_migrated \
              CLIENT_BUILD_DIR=${packages.ocs-client}/out ocs_api
            '';
          };
        };

        defaultApp = apps.run-end-to-end;

        ### DEVELOPMENT ENVIRONMENTS ###

        devShells = flake-utils.lib.flattenTree {
          ocs-client = ocs-client.devShell;
          ocs-server = ocs-server.devShell;
        };

        devShell = pkgs.mkShell {

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
            apps.run-archive-node
            apps.run-archive-database

            deploy-rs
          ];

          shellHook = ''
            runghc Tools/downloadArchiveDump.hs
          '';
        };

        ### DEPLOYMENT CONFIGURATION ###

        nixosConfigurations.staging = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [ ./system_configurations/staging/configuration.nix ];
        };

        deploy.nodes.staging = {
          hostname = "ec2-3-98-128-134.ca-central-1.compute.amazonaws.com";
          user = "root";

          sshOpts = [ "-p" "~/.ssh/osc-ssh.pem" ];

          profilesOrder = [ "system" "archive-database" "archive-node" "onchain-signalling" ];

          profiles = { 
            system = {
              user = "root";
              path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.staging;
            };

            archive-node = {
              user = "archive-node";
              path = deploy-rs.lib.x86_64-linux.activate.custom apps.run-archive-node
                "./bin/run-archive-node";
            };

            archive-database = {
              user = "postgres";
              path = deploy-rs.lib.x86_64-linux.activate.custom apps.run-archive-database
                "./bin/run-archive-database";
            };

            onchain-signalling = {
              user = "onchain-signalling";
              path = deploy-rs.lib.x86_64-linux.activate.custom apps.run-end-to-end 
                "./bin/run-end-to-end";
            };
          };
        };

        checks = builtins.mapAttrs (system: 
          deployLib: deployLib.deployChecks self.deploy
        ) deploy-rs.lib;
      }
    );
}