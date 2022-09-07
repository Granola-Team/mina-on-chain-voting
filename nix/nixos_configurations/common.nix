{
  ec2.hvm = true;

  services.openssh = { enable = true; };

  system.stateVersion = "22.06";
}