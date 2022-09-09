{ modulesPath, config, pkgs, ... }: 
let
  ocs = import ../../../../default.nix;
in rec {
  imports = [ "${modulesPath}/virtualisation/amazon-image.nix" ];
  ec2.hvm = true;

  networking.hostName = "staging";

  services.openssh.enable = true;

  environment.systemPackages = [
    ocs.defaultApp.x86_64-linux
  ];

  users.users = {
    archive-node = {
      isNormalUser = true;
      home = "/home/archive-node";
      description = "host user for Mina Archive Node";
      extraGroups = [ ];
    };

    onchain-signalling = {
      isNormalUser = true;
      home = "/home/onchain-signalling";
      description = "host user for Onchain-Signalling";
      password = "onchain-signalling";
    };
  };

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_10;
    port = 5432;
    enableTCPIP = true;
    authentication = pkgs.lib.mkOverride 10 ''
      local all all trust
      host all all 127.0.0.1/32 trust
      host all all ::1/128 trust
    '';
    initialScript = pkgs.writeText "backend-initScript" ''
      CREATE ROLE postgres WITH LOGIN PASSWORD 'postgres' CREATEDB;
      CREATE DATABASE archive_balances;
      CREATE DATABASE archive_balances_migrated;
      GRANT ALL PRIVILEGES ON DATABASE archive_balances TO postgres;
      GRANT ALL PRIVILEGES ON DATABASE archive_balances_migrated TO postgres;
    '';
  };

  systemd.services.onchain-signalling = {
    wantedBy = [ "multi-user.target" ]; 
    after = [ "network.target" "postgresql.service" ];
    description = "Start the OnChain-Signalling client and server";
    environment = {
      PGDATA = 
"/var/lib/postgresql/${config.services.postgresql.package.psqlSchema}";
      DBNAME = "archive_balances_migrated";
      USER = "postgres";
      DBPORT = "5432";
      PORT = "8080";
      HOST = "localhost";
      PASSWD = "postgres";
    };
    serviceConfig = {
      Type = "forking";
      User = "onchain-signalling";
      ExecStart = ''${ocs.defaultApp.x86_64-linux}/bin/run-end-to-end &'';         
    };
  };

  system.stateVersion = "22.05";
}