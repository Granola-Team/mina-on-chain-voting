{ pkgs, ocs-server, ocs-client }:
pkgs.writeShellApplication {
  name = "run-end-to-end";
  runtimeInputs = [
    ocs-server
    ocs-client
  ];
  text = ''
    run-temp-database &
    CLIENT_BUILD_DIR=${ocs-client}/out ocs_api
  '';
}