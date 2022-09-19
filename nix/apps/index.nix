{ pkgs, ocs-client, ocs-server, source }:
let
  appDependencies = with pkgs; [
    geos gdal
    # postgres with postgis support
    (postgresql.withPackages (p: [ p.postgis ]))

    (haskellPackages.ghcWithPackages (self: with haskellPackages; [
      curl xml tar zlib fused-effects megaparsec bytestring directory tmp-postgres json process
    ]))
  ];
in {
  apps = rec {
    clean-archive-backups = import ./clean-archive-backups.nix {
      inherit pkgs appDependencies;
    };

    download-archive-dump = import ./download-archive-dump.nix {
      inherit pkgs appDependencies;
    };

    run-end-to-end = import ./run-end-to-end.nix {
      inherit pkgs ocs-client ocs-server;
    };

    run-temp-database = import ./run-temp-database.nix {
      inherit pkgs appDependencies;
    };

    run-with-temp-database = import ./run-with-temp-database.nix {
      inherit pkgs run-temp-database ocs-client ocs-server;
    };

    deploy-ocs = import ./deploy-ocs.nix {
      inherit pkgs source;
      appDependencies = with pkgs; [ morph ];
    };
  };
}