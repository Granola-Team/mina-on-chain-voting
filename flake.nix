{
  description = "On Chain Signalling Deployment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/22.05";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    deploy-rs.url = "github:serokell/deploy-rs";
    mina.url = "github:MinaProtocol/mina";
  };

  outputs = { self, nixpkgs, flake-utils, flake-compat, deploy-rs, mina }: 
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in rec {

        deploy.nodes.onChain-signalling = {
          hostname = "35.203.38.140";

          profiles = { };
        };

        checks = builtins.mapAttrs (system: 
          deployLib: deployLib.deployChecks self.deploy
        ) deploy-rs.lib;

        devShell = pkgs.mkShell {

          buildInputs = with pkgs; [
            haskell-language-server
            rnix-lsp nixpkgs-fmt
            (haskellPackages.ghcWithPackages (self: with haskellPackages; [
              curl xml fused-effects megaparsec bytestring

            ]))
          ];

          shellHook = ''
          '';
        };
      }
    );
}
