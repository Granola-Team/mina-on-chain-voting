# Archive Node Developer Tools

<hr>

Some tools to make working with Mina Archive Nodes and their SQL dumps provided by O(1) a little easier

<hr>

## Download Archive Dump

downloads the latest archive dump for your selected network

* build: `nix-build`
* run: `./result/bin/download-archive-dump --help`

## Run Archive Node from Archive Dump

create a new postgres instance and initialize it using a Mina archive dump

* build: `nix-build`
* run: `./result/binarchive-database --help`