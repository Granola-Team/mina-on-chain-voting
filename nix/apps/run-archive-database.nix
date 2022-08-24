{ pkgs, appDependencies }:
pkgs.writeShellApplication {
  name = "run-archive-database";
  runtimeInputs = appDependencies;
  text = let
    postgresConf =
      pkgs.writeText "postgresql.conf"
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
  in ''
    echo "Using ${pkgs.postgresql.name}."

    # Setup: other env variables
    export PGHOST="$PGDATA"
    # Setup: DB
    [ ! -d "$PGDATA" ] && pg_ctl initdb -o "-U postgres" && cat "${postgresConf}" >> "$PGDATA"/postgresql.conf
  # shellcheck disable=SC2154,SC2139
    pg_ctl -o "-p '$archive-node-database-port' -k \"$PGDATA\"" start
    # alias fin="pg_ctl stop && exit"
    # alias pg="psql -p $archive-node-database-port -U postgres"

    download-archive-dump
    
  '';
}