{
  imports = [ ../common.nix ];

  environment.sessionVariables = {
    onchain-signalling-port = "8080";
    archive-node-database-port = "5555";
    PGDATA = "/home/postgres/";
    DBNAME = "archive_balances_migrated";
    USER = "postgres";
    DBPORT = "5432";
    PORT = "8080";
  };

  users.users = {
    postgres = {
      isNormalUser = true;
      home = "/home/postgres";
      description = "user for postgres";
      password = "postgres";
    };

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
    };
  };

  environment.systemPackages = [

  ];

  systemd.services.onchain-signalling = {
    wantedBy = [ "multi-user.target" ]; 
    after = [ "network.target" ];
    description = "Start the OnChain-Signalling client and server";
    serviceConfig = {
      Type = "forking";
      User = "onchain-signalling";
      ExecStart = ''${pkgs.screen}/bin/screen -dmS irc ${pkgs.irssi}/bin/irssi'';         
      ExecStop = ''${pkgs.screen}/bin/screen -S irc -X quit'';
    };
  };
}