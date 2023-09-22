{ pkgs ? import <nixpkgs> {} }:

let

  podmanSetupScript = let
    registriesConf = pkgs.writeText "registries.conf" ''
      [registries.search]
      registries = ['docker.io']
      [registries.block]
      registries = []
    '';
    storageConf = pkgs.writeText "storage.conf" ''
      [storage]
      driver = "overlay"
    '';
  in pkgs.writeScript "podman-setup" ''
    #!${pkgs.runtimeShell}
    if ! test -f ~/.config/containers/policy.json; then
      install -Dm555 ${pkgs.skopeo.src}/default-policy.json ~/.config/containers/policy.json
    fi
    if ! test -f ~/.config/containers/registries.conf; then
      install -Dm555 ${registriesConf} ~/.config/containers/registries.conf
    fi
    if ! test -f ~/.config/containers/storage.conf; then
      install -Dm555 ${storageConf} ~/.config/containers/storage.conf
    fi
  '';

in pkgs.mkShell {

  buildInputs = [
    pkgs.cargo-make  # For running 'cargo make' tasks.
    pkgs.clippy      # For testing Rust code.
    pkgs.just        # For running the build tool.
    pkgs.nodejs      # Required for running 'web'.
    pkgs.openssl     # Required for compiling.
    pkgs.pkg-config  # Required for compiling.
    pkgs.podman      # Required for testing with containers.
    pkgs.postgresql  # Required for compiling against libpq.
    pkgs.skopeo

    # Not on Darwin:
    #
    # pkgs.runc  # Container runtime
    # pkgs.conmon  # Container runtime monitor
    pkgs.slirp4netns  # User-mode networking for unprivileged namespaces

    # CoW for images, much faster than vfs, but default "overlay" is faster
    # still.
    # pkgs.fuse-overlayfs  # CoW for images, much faster than vfs, but default
  ];

  shellHook = ''
    # Install required configuration
    ${podmanSetupScript}
  '';
}
