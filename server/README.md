# Contributing to the On Chain Signalling API (ocs_api)

The On Chain Signalling API is a
REST API that fetches and parses
signalling information from transactions
recorded on a Mina Archive Node for consumption
by the On Chain Signalling Client

## Development Environment

- `nix develop .#server`

Run from the monorepo directory, and ensure
Nix Flakes are enabled
https://nixos.wiki/wiki/Flakes#Installing_nix_flakes

# Directory layout

> Use short lowercase names at least for the top-level files and folders.

    .
    ├── target                  # Compiled files
    ├── src                     # Source files
    ├── test                    # Automated tests
    ├── LICENSE
    └── README.md

## Dependencies

- `Axum` - Web framework
- `Tokio` - An event-driven, non-blocking I/O platform
- `SQLX` - Rust SQL Toolkit
- `Postgres` - Postgres Rust interface

**Prerequisites & Environment Variables**

- `DATABASE_URL` - Primary DB connection string
- `MAINNET_DATABASE_URL` - Mina Mainnet DB connection string
- `DEVNET_DATABASE_URL` - Mina Devnet DB connection string
- `LEDGER_PATH` - Path to the most recent staking ledger
- `CLIENT_PATH` - Path to the most recent frontend build

## Usage

**Development**

`cargo run -- start`

- Frontend served @ `http://localhost:8080`
- API served @ `http://localhost:8080/api/v1/*`

**Production**

`cargo build --release`
`cargo run --release -- start`

- Outputs a production build to `/target`
- Frontend served @ `http://localhost:8080`
- API served @ `http://localhost:8080/api/v1/*`
