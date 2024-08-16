# See status.nixos.org for options for this commit.
{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/311a4be96d940a0c673e88bd5bc83ea4f005cc02.tar.gz") {}
}:

pkgs.mkShell {

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
  ] ++ pkgs.lib.optionals (pkgs.stdenv.isDarwin) [
    pkgs.darwin.apple_sdk.frameworks.Security
    pkgs.darwin.apple_sdk.frameworks.SystemConfiguration
  ];

  shellHook = ''
    # To avoid running out of space
    # export TMPDIR=/var/tmp
  '';
}
