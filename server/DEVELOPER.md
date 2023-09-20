# Development of the 'server' Component

- If not installed, install 'libpq' (which is required by Diesel). On some
  Linux distros, this is accomplished, for example, by issuing:

  ```bash
  sudo apt-get install libpq-dev
  ```

- If not installed, install each of these as shown below:
  - [Rust](https://www.rust-lang.org/)
  - [Cargo-Make](https://github.com/sagiegurari/cargo-make)
  - [Diesel-CLI](https://crates.io/crates/diesel_cli/2.0.1)
  - Cargo-Audit

  ```bash
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
  cargo install --force cargo-make
  cargo install diesel_cli --no-default-features --features postgres
  cargo install cargo-audit

  ```


## Building, Linting, and Testing

To audit the Rust code:

```bash
cargo audit
```

To lint the Rust code:

```bash
cargo make clippy
```

Build and unit-test the Rust code:

```bash
cargo make
```

## Running in Docker

Run `docker-compose --profile all up` to mount the cluster, and then run all
pending migrations.

- Make sure the `DATABASE_URL`, the connection URL for the application
  database, and `ARCHIVE_DATABASE_URL`, the connection URL for the archive in
  your .env file correspond to those in Docker, especially if you are changing
  these environment variables.

> **IMPORTANT:**
When running locally, modify the respective variables in the `.env` file to
point to `db` and `server` (the internal Docker host).


## Database Migrations

The development database is mounted in Docker and managed via the
[Diesel CLI](https://diesel.rs/guides/getting-started) from the `server`
directory.

- `diesel database reset` — reset the database (**all data will be wiped
  out!**)

- `diesel database setup` — create the database if it doesn't exist and run all
  migrations.

- `diesel migration generate [name]` — create a new migration for changes to
  the schema.

For more commands and options, see [the official docs.](https://crates.io/crates/diesel_cli)


## Resources

- [Rust Programming Language](https://doc.rust-lang.org/book/)
