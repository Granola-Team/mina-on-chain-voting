use std::sync::Arc;
use sqlx::PgPool;

pub mod constants;
pub mod queries;
pub mod models;
pub mod routes;
pub mod ledger;

#[macro_use]
extern crate postgres_derive;

#[derive(Clone)]
pub struct ApiContext {
    pub config: Arc<Config>,
    pub ledger: Arc<tokio_rusqlite::Connection>,
    pub db: PgPool,
}

#[derive(clap::Parser)]
pub struct Config {
    /// The connection URL for the Postgres database this application should use.
    #[clap(long, env)]
    pub database_url: String,
    /// The path to the current frontend build.
    #[clap(long, env)]
    pub client_path: String,
    /// The path to the current staking ledger.
    #[clap(long, env)]
    pub ledger_path: String,
    #[clap(subcommand)]
    pub subcmd: SubCommand,
}

#[derive(clap::Parser, Debug)]
pub enum SubCommand {
    /// Starts the server
    Start,
}
