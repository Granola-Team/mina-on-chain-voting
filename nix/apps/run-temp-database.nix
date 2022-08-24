{ pkgs, appDependencies }:
pkgs.writeShellApplication {
  name = "run-temp-database";
  runtimeInputs = appDependencies;
  text = "runghc ./Tools/runTempDatabase.hs";
}