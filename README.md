# On-Chain Voting

On-Chain Voting for Mina Protocol - Monitors blocks and its
transactions using memo variables. Folders added represent each
component needed for this project.

# Server & Client

An actix_web server that communicates with a MINA archive node to
provide signalling information, and to host a React application that
displays said info.  A React SPA that displays and totals signalling
results (votes from the API).

- `nix build` Builds the server and client and outputs to `./result`
- `nix develop .#server` -> Enter a development shell for the server
- `nix develop .#client` -> Enter a development shell for the client
- `nix develop` -> Enter a development shell for this monorepo, with
  access to its development tools in the path

Environment Variables for `nix run .`: These are necessary when
running OnChain-Voting manually (i.e. during local dev)

- `DBNAME` : the name of the mina archive database
  (`archive_balances_migrated`)
- `USER` : the user that owns the postgres database (`postgres`)
- `PASSWD` : the database password (`postgres`)
- `DBPORT` : the database's bound port (`5432`)
- `PORT`
: the port to bind the on-chain-signalling server to (`8080`)
- `HOST` : host ip for the postgres database (`localhost`)

(default environment variables conform to test database expectations)

# Tools

A set of scripts to automate development environment management and
deployment. These scripts are automatically added to your PATH when
entering the shell.nix

- `run-temp-database` -- Provisions and runs a temporary postgres
  database to help with end-to-end testing in a local environment

- `download-archive-dump` -- Automatically downloads and extracts the
  latest MINA archive dump from MINA's google cloud storage API

- `clean-archive-backups` -- Removes old archive dumps from the local
  environment

- `run-end-to-end` -- Builds and starts the actix-web server and links
  it to the latest build of the frontend in the nix store

# License (See LICENSE file for full license)

Copyright 2022-2023 Granola Systems Inc.

Free use of this software is granted under the terms of the Mozilla
Public License 2.0.

