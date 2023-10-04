source $stdenv/setup

buildPhase() {
  echo "building archive node tools..."
  buildArchiveDatabase
  buildArchiveDumpDownloader
}

buildArchiveDatabase() {
    ghc --make ArchiveDatabase -o archive-database
    rm ./**/*.o
    rm ./**/*.hi
    rm *.hi
    rm *.o
}

buildArchiveDumpDownloader() {
    ghc --make DownloadArchiveDump -o download-archive-dump
    rm ./**/*.o
    rm ./**/*.hi
    rm *.hi
    rm *.o
}

installPhase() {
  mkdir -p $out/bin
  cp archive-database $out/bin
  cp download-archive-dump $out/bin
}

genericBuild