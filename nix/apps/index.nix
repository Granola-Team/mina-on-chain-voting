{ pkgs, ocs-client, ocs-server }:
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
  apps = {
    clean-archive-backups = import ./clean-archive-backups.nix {
      inherit pkgs appDependencies;
    };

    download-archive-dump = import ./download-archive-dump.nix {
      inherit pkgs appDependencies;
    };

    run-archive-node = import ./run-archive-node.nix {
      inherit pkgs;
    };

    run-archive-database = import ./run-archive-database.nix {
      inherit pkgs appDependencies;
    };

    run-end-to-end = import ./run-end-to-end.nix {
      inherit pkgs ocs-client ocs-server;
    };

    run-temp-database = import ./run-temp-database.nix {
      inherit pkgs appDependencies;
    };
  };
}