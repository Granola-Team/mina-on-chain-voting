{ pkgs, serverSource, serverDependencies }:
devShell = pkgs.mkShell {
  buildInputs = serverDependencies;
  shellHook = ''
    cd ${serverSource}
    cargo check && cargo test
  '';
}