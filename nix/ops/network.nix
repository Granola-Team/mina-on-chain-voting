{
  network = {
    description = "OCS Deployment Network";
  };

  "staging" = {config, pkgs, lib, ...}: {
    imports = [
      ../hosts/staging.nix
    ];

    # The user you will SSH 
    # into the machine as
    deployment.targetUser = "root";

    # The target IP address or hostname
    # of the server we are deploying to
    deployment.targetHost = "ec2-3-98-128-134.ca-central-1.compute.amazonaws.com";
  };
}