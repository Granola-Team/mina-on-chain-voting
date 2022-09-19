{ pkgs, appDependencies, source }:
pkgs.writeShellApplication {
  name = "deploy-ocs";
  runtimeInputs = appDependencies;
  text = ''
    SSH_IDENTITY_FILE=~/.ssh/ocs-ssh.pem \
    morph deploy ${source}/nix/ops/network.nix "$1"
  '';
}