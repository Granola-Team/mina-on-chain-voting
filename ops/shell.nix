{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/261abe8a44a7e8392598d038d2e01f7b33cf26d0.tar.gz") {}
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

  RUSTC_VERSION = pkgs.lib.readFile ../rust-toolchain;

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
    pkgs.clang
    pkgs.just        # For running the build tool.
    pkgs.libiconv    # Required for compiling Rust tools.
    pkgs.llvmPackages.bintools
    pkgs.nodePackages.pnpm
    pkgs.openssl     # Required for compiling.
    pkgs.pkg-config  # Required for compiling.
    pkgs.postgresql  # Required for compiling against libpq.
    pkgs.rustup
    pkgs.skopeo
  ] ++ pkgs.lib.optionals (!pkgs.stdenv.isDarwin) [
    pkgs.runc     # Container runtime
    # pkgs.conmon   # Container runtime monitor
    pkgs.slirp4netns  # User-mode networking for unprivileged namespaces
    pkgs.podman       # Required for testing with containers.
  ] ++ pkgs.lib.optionals (pkgs.stdenv.isDarwin) [
    pkgs.darwin.apple_sdk.frameworks.Security
    pkgs.darwin.apple_sdk.frameworks.SystemConfiguration
  ];

  shellHook = ''
    export PATH=$PATH:''${CARGO_HOME:-~/.cargo}/bin
    export PATH=$PATH:''${RUSTUP_HOME:-~/.rustup}/toolchains/$RUSTC_VERSION-x86_64-unknown-linux-gnu/bin/
    # Install required configuration
    ${podmanSetupScript}
  '';
}
