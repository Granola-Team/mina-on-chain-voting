# See status.nixos.org for options for this commit.
{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/311a4be96d940a0c673e88bd5bc83ea4f005cc02.tar.gz") {}
}:

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
      graphRoot = "$HOME/containers/graphRoot"
      runRoot = "$HOME/containers/runRoot"
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

  # Provides fake "docker" and "docker-compose" binaries.
  dockerCompat = pkgs.runCommandNoCC "docker-podman-compat" {} ''
    mkdir -p $out/bin
    ln -s ${pkgs.podman}/bin/podman $out/bin/docker
    ln -s ${pkgs.podman-compose}/bin/podman-compose $out/bin/docker-compose
  '';

in pkgs.mkShell {

  # See https://github.com/rust-lang/rust-bindgen#environment-variables

  LIBCLANG_PATH = pkgs.lib.makeLibraryPath [ pkgs.llvmPackages_latest.libclang.lib ];

  # Add precompiled library to rustc search path
  RUSTFLAGS = (builtins.map (a: ''-L ${a}/lib'') [
    pkgs.libiconv
  ]);

  # Add glibc, clang, glib and other headers to bindgen search path
#  BINDGEN_EXTRA_CLANG_ARGS = 
#    # Includes with normal include path
#    (builtins.map (a: ''-I"${a}/include"'') [
#      # add dev libraries here (e.g. pkgs.libvmi.dev)
#      pkgs.glibc.dev 
#    ])
#    # Includes with special directory paths
#    ++ [
#      ''-I"${pkgs.llvmPackages_latest.libclang.lib}/lib/clang/${pkgs.llvmPackages_latest.libclang.version}/include"''
#      ''-I"${pkgs.glib.dev}/include/glib-2.0"''
#      ''-I${pkgs.glib.out}/lib/glib-2.0/include/''
#    ];

  buildInputs = [
    pkgs.cacert
    pkgs.clang
    pkgs.just        # For running the build tool.
    pkgs.libiconv    # Required for compiling Rust tools.
    pkgs.llvmPackages.bintools
    pkgs.nodePackages.pnpm
    pkgs.openssl     # Required for compiling.
    pkgs.pkg-config  # Required for compiling.
    pkgs.postgresql  # Required for compiling against libpq, and for pg_isready.
    pkgs.cargo
    pkgs.clippy
    pkgs.skopeo
  ] ++ pkgs.lib.optionals (!pkgs.stdenv.isDarwin) [
    pkgs.podman          # Required for testing with containers.
    pkgs.podman-compose  # Required for testing with containers.
    dockerCompat
  ] ++ pkgs.lib.optionals (pkgs.stdenv.isDarwin) [
    pkgs.darwin.apple_sdk.frameworks.Security
    pkgs.darwin.apple_sdk.frameworks.SystemConfiguration
  ];

  shellHook = ''
    # Install required configuration
    ${podmanSetupScript}
    export TMPDIR=/var/tmp
  '';
}
