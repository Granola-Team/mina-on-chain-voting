source $stdenv/setup

buildPhase() {
  echo "building archive node tools..."
  buildArchiveDatabase
  buildArchiveDumpDownloader
  cleanup()
}

buildArchiveDatabase() {
    ghc --make ArchiveDatabase -o archive-database
}

buildArchiveDumpDownloader() {
    ghc --make DownloadArchiveDump -o download-archive-dump
}

buildDatabaseRestoration() {
  ghc --make ArchiveDatabaseRestoration -o restore-archive-db
}

installPhase() {
  mkdir -p $out/bin
  cp archive-database $out/bin
  cp download-archive-dump $out/bin
}

cleanup() {
  rm ./**/*.o
  rm ./**/*.hi
  rm *.hi
  rm *.o
}

genericBuild