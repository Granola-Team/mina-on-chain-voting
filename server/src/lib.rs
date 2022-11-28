use sqlx::PgPool;
use std::sync::Arc;

pub mod constants;
pub mod ledger;
pub mod models;
pub mod queries;
pub mod router;
pub mod processor;

#[macro_use]
extern crate postgres_derive;

/// Contains secrets and connections needed for processing signals
#[derive(Clone)]
pub struct ApiContext {
    /// Application config to use
    pub config: Arc<Config>,
    /// Staking Ledger for Mainnet
    pub mainnet_ledger: Arc<tokio_rusqlite::Connection>,
    /// Staking Ledger for Devnet
    pub devnet_ledger: Arc<tokio_rusqlite::Connection>,
    /// Archive Node DB Connection for Mainnet
    pub mainnet_db: PgPool,
    /// Archive Node DB Connection for Devnet
    pub devnet_db: PgPool,
}

/// Application configuration used to gather and store secrets and env vars
#[derive(clap::Parser, Debug)]
pub struct Config {
    /// The connection URL for the Postgres database this application should use.
    #[clap(long, env)]
    pub mainnet_database_url: String,
    #[clap(long, env)]
    pub devnet_database_url: String,
    /// The path to the current frontend build.
    #[clap(long, env)]
    pub client_path: String,
    /// The path to the mainnet staking ledger.
    #[clap(long, env)]
    pub mainnet_ledger_path: String,
    /// The path to the devnet staking ledger.
    #[clap(long, env)]
    pub devnet_ledger_path: String,
    #[clap(subcommand)]
    pub subcmd: SubCommand,
}

/// potential user actions the server can execute
#[derive(clap::Parser, Debug)]
pub enum SubCommand {
    /// Starts the server
    Start,
}
