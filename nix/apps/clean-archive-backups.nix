{ pkgs, appDependencies }:
pkgs.writeShellApplication {
  name = "clean-archive-backups";
  runtimeInputs = appDependencies;
  text = "runghc ./Tools/cleanArchiveDumps.hs";
}