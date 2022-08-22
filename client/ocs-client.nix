{ nixpkgs, system }:
let
  pkgs = import nixpkgs { inherit system; };
  clientDependencies = with pkgs; [
    yarn rnix-lsp nixpkgs-fmt
  ];
in {
  defaultPackage = pkgs.mkYarnPackage {
    name = "ocs-client";
    version = "1.0.0";
    src = ./.;
    buildPhase = ''
      yarn build
      mkdir -p $out/out
    '';
    distPhase = "true";
    installPhase = ''
      cp -r deps/ocs-client/build/* $out/out/
    '';
  };

  devShell = pkgs.mkShell {
    buildInputs = clientDependencies;
    shellHook = ''
      cd client
      yarn install
    '';
  };
}