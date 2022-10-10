{ pkgs, serverDependencies }:
pkgs.mkShell {
  buildInputs = serverDependencies;
  shellHook = ''
    cd ./server
    set -a; source .env; set +a
    cargo check && cargo test
  '';
}