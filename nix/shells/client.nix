{ pkgs, clientSource, clientDependencies }:
pkgs.mkShell {
  buildInputs = clientDependencies;
  shellHook = ''
    cd ${clientSource}
    yarn install
  '';
}