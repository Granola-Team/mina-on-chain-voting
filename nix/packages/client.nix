{ pkgs, clientSource, clientDependencies }:
pkgs.mkYarnPackage {
    name = "ocs-client";
    version = "1.0.0";
    src = clientSource;
    buildPhase = ''
      yarn build
      mkdir -p $out/out
    '';
    distPhase = "true";
    installPhase = ''
      cp -r deps/ocs-client/build/* $out/out/
    '';
}