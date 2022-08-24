{ self, nixpkgs, flake-utils, flake-compat, deploy-rs-flake, rust-overlay, sourceDir }: 
  flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      overlays = [ (import rust-overlay) ];

      pkgs = import nixpkgs { inherit system overlays; };

      rust = pkgs.rust-bin.stable.latest.default.override {
        extensions = [ "rust-src" ];
      };

      packages-index = import ./packages/index.nix {
        inherit pkgs;
        defaultSource = sourceDir;
      };

      apps-index = import ./apps/index.nix {
        inherit pkgs;
        ocs-client = packages-index.packages.client;
        ocs-server = packages-index.packages.server;
      };

      shells-index = import ./shells/index.nix {
        inherit pkgs;
        defaultSource = sourceDir;
        apps = apps-index.apps;
      };

      deploy-rs = deploy-rs-flake.defaultPackage.${system};

    in rec {

      ### BUILD CONFIGURATION ###

      packages = packages-index.packages;

      ### APPLICATIONS USING FLAKE BINARIES ###

      apps = apps-index.apps;

      defaultApp = apps.run-end-to-end;

      ### DEVELOPMENT ENVIRONMENTS ###

      devShells = shells-index.devShells;

      ### DEPLOYMENT CONFIGURATION ###

      nixosConfigurations.staging = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ ./nixos_configurations/staging/configuration.nix ];
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
  )