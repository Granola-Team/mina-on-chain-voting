{ pkgs, serverSource, serverDependencies, rustPlatform }:
rustPlatform.buildRustPackage rec {
  pname = "ocs_api";
  version = "1.0.0";
  nativeBuildInputs = serverDependencies;

  src = serverSource;

  cargoLock = { lockFile = "${serverSource}/Cargo.lock"; };

  verifyCargoDeps = true;
}