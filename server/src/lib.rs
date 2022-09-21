use sqlx::PgPool;
use std::sync::Arc;

pub mod constants;
pub mod ledger;
pub mod models;
pub mod queries;
pub mod routes;

#[macro_use]
extern crate postgres_derive;

#[derive(Clone)]
pub struct ApiContext {
    pub config: Arc<Config>,
    pub ledger: Arc<tokio_rusqlite::Connection>,
    pub mainnet_db: PgPool,
    pub devnet_db: PgPool,
}

#[derive(clap::Parser)]
pub struct Config {
    /// The connection URL for the Postgres database this application should use.
    #[clap(long, env)]
    pub mainnet_database_url: String,
    #[clap(long, env)]
    pub devnet_database_url: String,
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
