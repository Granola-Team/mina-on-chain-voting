{ pkgs, appDependencies }:
pkgs.writeShellApplication {
  name = "download-archive-dump";
  runtimeInputs = appDependencies;
  text = "runghc ./Tools/downloadArchiveDump.hs";
}