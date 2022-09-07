# ops/home/network.nix

{
  # Mara\ Configuration for the network in general.
  network = {
    # Mara\ A human-readable description.
    description = "OCS Deployment Network";
  };

  "staging" = {config, pkgs, lib, ...}: {
    imports = [
      ../../hosts/staging/configuration.nix
    ];

    # Mara\ The user you will SSH into the
    # machine as. This defaults to your current
    # username, however for this example we will
    # just SSH in as root.
    deployment.targetUser = "root";

    # Mara\ The target IP address or hostname
    # of the server we are deploying to. This is
    # the IP address of a libvirtd virtual
    # machine on my machine.
    deployment.targetHost = "ec2-3-98-128-134.ca-central-1.compute.amazonaws.com";
  };
}