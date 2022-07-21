let
nixpkgs = import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/22.05.tar.gz) {
  overlays = [];
  config = {};
};

in
with nixpkgs;

stdenv.mkDerivation {
  name = "postgres-env";
  buildInputs = [];

  nativeBuildInputs = [
    zsh
    vim
    geos
    gdal
    nixpkgs-fmt
    (python38.withPackages (ps: with ps; [ lxml pycurl certifi beautifulsoup4 ]))
    # postgres-12 with postgis support
    (postgresql.withPackages (p: [ p.postgis ]))
  ];

  postgresConf =
    pkgs.stdenv.writeText "postgresql.conf"
      ''
        # Add Custom Settings
        log_min_messages = warning
        log_min_error_statement = error
        log_min_duration_statement = 100  # ms
        log_connections = on
        log_disconnections = on
        log_duration = on
        #log_line_prefix = '[] '
        log_timezone = 'UTC'
        log_statement = 'all'
        log_directory = 'pg_log'
        log_filename = 'postgresql-%Y-%m-%d_%H%M%S.log'
        logging_collector = on
        log_min_error_statement = error
      '';


  # ENV Variables
  LD_LIBRARY_PATH = "${geos}/lib:${gdal}/lib";
  PGDATA = "${toString ./.}/.pg";

  # Post Shell Hook
  shellHook = ''
    echo "Using ${postgresql.name}."
    export DUMP_NAME=$(python3 download_database_dump.py)
    tar -xvzf ./database_dumps/$DUMP_NAME.tar.gz --directory ./database_dumps
    # Setup: other env variables
    export PGHOST="$PGDATA"
    # Setup: DB
    [ ! -d $PGDATA ] && pg_ctl initdb -o "-U postgres --no-locale --encoding=UTF8" && cat "$postgresConf" >> $PGDATA/postgresql.conf
    pg_ctl -o "-p 5555 -k $PGDATA" start
    alias fin="pg_ctl stop && exit"
    alias pg="psql -p 5555 -U postgres"
    pg << EOF
      CREATE DATABASE archive;
      CREATE DATABASE archive_balances_migrated;
    EOF
    pg -f database_dumps/$DUMP_NAME
  '';
}