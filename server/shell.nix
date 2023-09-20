{ pkgs ? import <nixpkgs> {} }:

let

  podmanSetupScript = let
    registriesConf = pkgs.writeText "registries.conf" ''
      [registries.search]
      registries = ['docker.io']
      [registries.block]
      registries = []
    '';
  in pkgs.writeScript "podman-setup" ''
    #!${pkgs.runtimeShell}
    if ! test -f ~/.config/containers/policy.json; then
      install -Dm555 ${pkgs.skopeo.src}/default-policy.json ~/.config/containers/policy.json
    fi
    if ! test -f ~/.config/containers/registries.conf; then
      install -Dm555 ${registriesConf} ~/.config/containers/registries.conf
    fi
  '';

in pkgs.mkShell {

  buildInputs = [
    pkgs.cargo-make
    pkgs.clippy
    pkgs.openssl
    pkgs.pkg-config
    pkgs.podman
    pkgs.postgresql
    pkgs.skopeo

    # Not on Darwin:
    #
    # pkgs.runc  # Container runtime
    # pkgs.conmon  # Container runtime monitor
    # pkgs.slirp4netns  # User-mode networking for unprivileged namespaces
    # pkgs.fuse-overlayfs  # CoW for images, much faster than default vfs
  ];

  shellHook = ''
    # Install required configuration
    ${podmanSetupScript}
  '';
}
