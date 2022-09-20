{ pkgs, clientDependencies }:
pkgs.mkShell {
  buildInputs = clientDependencies;
  shellHook = ''
    cd ./client
    yarn install
  '';
}