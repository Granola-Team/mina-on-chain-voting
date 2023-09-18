# Development

## Make sure to have the necessary installations and dependencies

- If not installed, install [`pnpm`](https://pnpm.io/)

  ```bash
  brew install pnpm

  # or ...

  curl -fsSL https://get.pnpm.io/install.sh | sh -
  ```

- Set the NodeJS version to be used:

  ```bash
  pnpm env use --global 18
  ```

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

## Start developing

- Checkout this repository via `git` or the [Github CLI](https://cli.github.com/)

  ```bash
  git clone git@github.com:Granola-Team/mina-on-chain-voting.git

  # or ...

  gh repo clone Granola-Team/mina-on-chain-voting
  ```

- In the new directory, install dependencies

  ```bash
  pnpm clean && pnpm install
  ```

- Make sure your .env file is set-up correctly

  Please see the [`.env.example`](./.env.example) file in the root of the project for more details.

## Building, Linting, and Testing

### Rust

Rust code is in the './server' directory. From that directory:

To audit the Rust code:

```bash
cargo audit
```

To lint the Rust code, from the `server` directory:

```bash
cargo make clippy
```

Lint-and-unit-test the Rust code, from the `server` directory:

```bash
cargo make --profile ci-flow
```

### Front End 

Lint the front end (web):

```bash
pnpm web ts-lint
```

Test the front end (web):

```bash
pnpm web test
```

Building the front end (web):

```bash
pnpm web build
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

## Running in the console

You can run the web-app in console and mount only both the database and the
server in Docker when developing the web app locally.

> **IMPORTANT:** When running this way, the database URL in the `.env` file has to point to `localhost`.</br>
See [`.env.example`](./.env.example) for more information on the `DATABASE_URL` env var.

- Mount the database and server in Docker. The db and backend should be up and running now.

  ```sh
  docker-compose --profile server-db up
  ```

- Run migrations from the `server` directory.

  ```sh
  diesel migration run
  ```

- Run the app (frontend) in development mode.

  ```sh
  pnpm web dev
  ```

## Managing the database and migrations

The development database is mounted in Docker and managed via the
[Diesel CLI](https://diesel.rs/guides/getting-started) from the `server`
directory.

- `diesel database reset` — reset the database (**all data will be wiped out!**)

- `diesel database setup` — create the database if it doesn't exist and run all migrations.

- `diesel migration generate [name]` — create a new migration for changes to the schema.

For more commands and options, see [the official docs.](https://crates.io/crates/diesel_cli)

## Resources

- [Next.js Documentation](https://nextjs.org/docs/getting-started)
- [Rust Programming Language](https://doc.rust-lang.org/book/)
- [Typescript](https://www.typescriptlang.org/docs/)
