{ pkgs }:
let
  # libraries for patchelf
  rpath = pkgs.lib.makeLibraryPath (with pkgs; [
    postgresql
    dpkg
    openssl
    gmp gomp
    tzdata libffi
    jemalloc
    procps
    zlib
    bzip2
    libgccjit # maybe replace if linker shows too much gcc
    libffi_3_3
  ]);
in pkgs.stdenv.mkDerivation {
  name = "mina-mainnet";

  # -------------------- Update this when Mina Updates ----------------------------------------
  src = pkgs.fetchurl {
    url = "https://packages.o1test.net/pool/bullseye/m/mi/mina-mainnet_1.3.1.1-f361ba1.deb";
    sha256 = "24f5948521bb077dc034a730c5de916a0896c037cd5c07dec66da8c6fa3230c1";
  };
  # -------------------------------------------------------------------------------------------

  sourceRoot = ".";
  unpackCmd = "${pkgs.dpkg}/bin/dpkg-deb -x $src .";

  # We aren't compiling anything, just packageing binaries
  dontConfigure = true;
  dontBuild = true;

  nativeBuildInputs = with pkgs; [
    autoPatchelfHook
    wrapGAppsHook
  ];

  installPhase = ''
    mkdir -p $out
    cp -r ./usr/local/bin/ $out/
  '';

  # make sure all lib dependencies are linked properly
  fixupPhase = ''
    for binary in $out/bin/*; do
      patchelf \
        --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
        --set-rpath "${rpath}" \
        "$binary"
    done
  '';
}
