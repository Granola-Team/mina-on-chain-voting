{ modulesPath, ... }: {
  imports = [ "${modulesPath}/virtualisation/amazon-image.nix" ];
  ec2.hvm = true;

  environment.sessionVariables = {
    onchain-signalling-port = 8080;
    archive-node-database-port = 5555;
    PGDATA = "/home/postgres/";
  };

  users = {
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