# On-Chain Signalling

On-Chain Signalling for Mina Protocol - Monitors blocks and its transactions using memo variables.\
Folders added represent each component needed for this project.

# Server & Client

An actix_web server that communicates with a MINA archive node to provide signalling information, and to host a React application that displays said info.\
A React SPA that displays and totals signalling results (votes) from the API.

- `nix-build` or `nix build` (to use Flakes) -> Builds the server and client and outputs to `./result`
- `nix-shell ./{client, server}/shell.nix` or `nix develop .#{ocs-client, ocs-server}` -> Enter a development shell ready for contributing to the client or server
- `nix-shell .` or `nix develop` -> Enter a development shell for this monorepo, with access to its development tools in the path

Environment Variables for `nix run .`:

- DBNAME : the name of the mina archive database (`archive_balances_migrated`)
- USER : the user that owns the postgres database (`$USER`)
- PASSWD : the database password (`''`)
- DBPORT : the database's bound port (`5432`)
- PORT : the port to bind the on-chain-signalling server to (`8080`)

# Tools

A set of scripts to automate development environment management and deployment. These scripts are automatically added to your PATH when entering the shell.nix

- `run-temp-database` --
  Provisions and runs a temporary postgres database to help with end-to-end testing in a local environment

- `download-archive-dump` --
  Automatically downloads and extracts the latest MINA archive dump from MINA's google cloud storage API

- `clean-archive-backups` --
  Removes old archive dumps from the local environment

- `run-onchain-signalling` --
  Builds and starts the actix-web server and links it to the latest build of the frontend in the nix store
