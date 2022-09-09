{ modulesPath, config, ... }: 
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

    postgres = {
      password = "postgres";
    };
  };

  # systemd.services.archive-database = {
  #   wantedBy = [ "multi-user.target" ]; 
  #   after = [ "network.target" ];
  #   description = "Start the OnChain-Signalling Archive Database";
  #   serviceConfig = {
  #     Type = "forking";
  #     User = "postgres";
  #     ExecStart = ''${ocs.apps.run-temp-database}/bin/run-temp-database'';         
  #   };
  # };

  services.postgresql = {
    port = 5432;
    enable = true;
    ensureUsers = [
      {
        name = "postgres";
        ensurePermissions = {
          "ALL TABLES IN SCHEMA public" = "ALL PRIVILEGES";
        };
      }
    ];
    authentication = ''
      # TYPE    DATABASE                  USER      ADDRESS   METHOD
        local   all                       postgres            password
    '';
    ensureDatabases = [
      "archive_balances_migrated"
      "archive_balances"
    ];
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
      ExecStart = ''${ocs.defaultApp.x86_64-linux}/bin/run-end-to-end'';         
    };
  };

  system.stateVersion = "22.05";
}