use sqlx::PgPool;
use std::sync::Arc;

pub mod constants;
pub mod ledger;
pub mod models;
pub mod processor;
pub mod queries;
pub mod router;
pub mod types;

#[macro_use]
extern crate postgres_derive;

#[derive(Clone)]
pub struct APIContext {
    pub config: Arc<Config>,
    pub mainnet_db: PgPool,
}

#[derive(clap::Parser)]
pub struct Config {
    /// The connection URL for the Postgres database this application should use.
    #[clap(long, env)]
    pub database_url: String,
    #[clap(long, env)]
    pub devnet_database_url: String,
    /// The path to the current frontend build.
    #[clap(long, env)]
    pub client_path: String,
    #[clap(subcommand)]
    pub subcmd: SubCommand,
}

#[derive(clap::Parser, Debug)]
pub enum SubCommand {
    /// Starts the server
    Start,
}
