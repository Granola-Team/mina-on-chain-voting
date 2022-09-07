{ modulesPath, ... }: {
  imports = [ "${modulesPath}/virtualisation/amazon-image.nix" ];
  ec2.hvm = true;

  networking.hostname = "onchain-signalling-ec2";

  environment.sessionVariables = {
    onchain-signalling-port = 8080;
    archive-node-database-port = 5555;
    PGDATA = "/home/postgres/";
    DBNAME = "archive_balances_migrated";
    USER = "postgres";
    DBPORT = "5432";
    PORT = "8080";
  };

  users.users = {
    postgres = {
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
}