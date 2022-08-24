{ pkgs, ocs-server, ocs-client }:
pkgs.writeShellApplication {
  name = "run-end-to-end";
  runtimeInputs = [
    ocs-server
    ocs-client
  ];
  text = ''
  # shellcheck disable=SC2154,SC2139
    PORT="onchain-signalling-port" \
    DBPORT="$archive-node-database-port" \
    PASSWD=postgres \
    USER=postgres \
    DBNAME=archive_balances_migrated \
    CLIENT_BUILD_DIR=${ocs-client}/out ocs_api
  '';
}