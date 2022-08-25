{ pkgs, run-temp-database, ocs-server, ocs-client }:
pkgs.writeShellApplication {
  name = "run-with-temp-database";
  runtimeInputs = [
    ocs-server
    ocs-client
    run-temp-database
  ];
  text = ''
    run-temp-database &
    sleep 30
    CLIENT_BUILD_DIR=${ocs-client}/out ocs_api
  '';
}