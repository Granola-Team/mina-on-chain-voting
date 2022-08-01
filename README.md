# onchain-signalling
Onchain Signalling for Mina Protocol - Monitors blocks and its transactions using memo variables.
Folders added represent each component needed for this project 

# Server & Client
An actix_web server that communicates with a MINA archive node to provide signalling information, and to host a React application that displays said info.
A Bun/React SPA that displays and totals signalling results (votes) from the API.

<<<<<<< HEAD
* `nix flake update` -> Updates the server and client
* `nix run .` -> Builds the server and client and runs the server
=======
* `nix run .` -> Builds the server and client and runs the server
* `nix flake update` -> Updates the server and client **This command must be run every time the server / client are updated**
>>>>>>> bd80ea403471f7c53590050889248ffe19156561

Environment Variables for `nix run .`:
* DBNAME : the name of the mina archive database (`archive_balances_migrated`)
* USER : the user that owns the postgres database (`postgres`)
* PASSWD : the database password (`postgres`)
* DBPORT : the database's bound port (`5432`)
* PORT : the port to bind the on-chain-signalling server to (`8080`)


# Tools
A set of scripts to automate development environment management and deployment. These scripts are automatically added to your PATH when entering the shell.nix

* runTempDatabase -- `run-temp-database`
provisions and runs a temporary postgres database to help with end-to-end testing in a local environment

* downloadArchiveDump -- `download-archive-dump`
automatically downloads and extracts the latest MINA archive dump from MINA's google cloud storage API

* cleanArchiveDumps -- `clean-archive-backups`
removes old archive dumps from the local environment

* run-onchian-signalling -- `run-onchain-signalling`
builds and starts the actix-web server and links it to the latest build of the frontend in the nix store