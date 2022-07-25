# onchain-signalling
Onchain Signalling for Mina Protocol - Monitors blocks and its transactions using memo variables.
Folders added represent each component needed for this project 

# server
An actix_web server that communicates with a MINA archive node to provide signalling information, and to host a React application that displays said info

# client
A Bun/React SPA that displays and totals signalling results (votes) from the api

# Tools
A set of scripts to automate development environment management and deployment

* runTempDatabase -- `nix run .` or `runghc Tools/runTempDatabase.hs`
provisions and runs a temporary postgres database to help with end-to-end testing in a local environment

* downloadArchiveDump -- `runghc Tools/downloadArchiveDump.hs`
automatically downloads and extracts the latest MINA archive dump from MINA's google cloud storage API

* cleanArchiveDumps -- `runghc Tools/cleanArchiveDumps.hs`
removes old archive dumps from the local environment
