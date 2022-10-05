{ self, nixpkgs, flake-utils, flake-compat, rust-overlay, sourceDir }: 
  flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ] (system:
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
        apps = apps-index.apps;
      };

    in rec {

      ### BUILD CONFIGURATION ###

      packages = packages-index.packages;

      ### APPLICATIONS USING FLAKE BINARIES ###

      apps = apps-index.apps;

      defaultApp = apps.run-end-to-end;

      ### DEVELOPMENT ENVIRONMENTS ###

      devShells = shells-index.devShells;
    }
  )