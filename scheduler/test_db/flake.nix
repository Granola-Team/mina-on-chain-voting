{
  description = "start a test postgres database";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/22.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        env = pkgs.writeShellScriptBin "env" ''
          export PGHOST=test_db/db
          export PGDATA=$PGHOST/data
          export PGDATABASE=postgres
          export PGLOG=$PGHOST/postgres.log
        '';

        dependencies = with pkgs; [
          postgresql env
        ];

      in pkgs.stdenv.mkDerivation rec {
        apps = {
          create_db = pkgs.writeShellApplication {
            name = "create_db";
            runtimeInputs = dependencies;
            text = ''
              # shellcheck disable=SC1091
              source ${env}/bin/env

              mkdir -p "$PGHOST"
              if [ ! -d "$PGDATA" ]; then
                initdb -U postgres --auth=trust --no-locale --encoding=UTF8
              fi

              if ! pg_ctl -U postgres status
              then
                pg_ctl -U postgres start -l "$PGLOG" -o "--unix_socket_directories='$PGHOST'"
              fi
            '';
          };

          start_db = pkgs.writeShellApplication {
            name = "start_db";
            runtimeInputs = dependencies;
            text = ''
              . /test_db/env.sh;

              mkdir -p "$PGHOST"

              if [ ! -d "$PGDATA" ]; then
                initdb -U "$USER" --auth=trust --no-locale --encoding=UTF8
              fi

              if ! pg_ctl -U "$USER" status
              then
                pg_ctl -U "$USER" start -l "$PGLOG" -o "--unix_socket_directories='$PGHOST'"
              fi
            '';
          };

          stop_db = pkgs.writeShellApplication {
            name = "stop_db";
            runtimeInputs = dependencies;
            text = ''
              export PGHOST="db"
              export PGDATA="$PGHOST/data"
              export PGDATABASE=postgres
              export PGLOG="$PGHOST/postgres.log"

              pg_ctl stop && exit
            '';
          };

          drop_db = pkgs.writeShellApplication {
            name = "drop_db";
            runtimeInputs = dependencies;
            text = ''

            '';
          };
        };

        defaultApp = apps.create_db;
      }
    ); 
}