# Archive Node Developer Tools

<hr>

Some tools to make working with Mina Archive Nodes and their SQL dumps provided by O(1) a little easier

<hr>

## Download Archive Dump

downloads the latest archive dump for your selected network from Mina's `network_block_data` google cloud bucket

* build: `nix-build`
* run: `./result/bin/download-archive-dump --help`

## Run Archive Node from Archive Dump

Initialize a new PostgreSQL instance, restore it with a downloaded Archive Dump, and run it using libpq. The database isntance is initialized in a folder specified by a command line argument and can be set to be deleted or retained on shutdown

* build: `nix-build`
* run: `./result/bin/archive-database --help`