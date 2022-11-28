use anyhow::Context;
use ledger::{HasConnectionAsync, Ledger};
use mockall::automock;
use router::QueryRequestFilter;
use sqlx::{postgres::PgPoolOptions, PgPool};
use std::sync::Arc;

pub mod constants;
pub mod ledger;
pub mod models;
pub mod processor;
pub mod queries;
pub mod router;

#[macro_use]
extern crate postgres_derive;

#[derive(Clone)]
pub struct ApiContext {
    pub config: Arc<Config>,
    pub mainnet_ledger: Arc<tokio_rusqlite::Connection>,
    pub devnet_ledger: Arc<tokio_rusqlite::Connection>,
    pub mainnet_db: PgPool,
    pub devnet_db: PgPool,
}

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

#[derive(clap::Parser, Debug)]
pub enum SubCommand {
    /// Starts the server
    Start,
}
