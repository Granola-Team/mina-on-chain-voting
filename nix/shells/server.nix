{ pkgs, serverDependencies }:
pkgs.mkShell {
  buildInputs = serverDependencies;
  shellHook = ''
    cd ./server
    cargo check && cargo test
  '';
}