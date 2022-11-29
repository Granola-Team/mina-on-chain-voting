use ledger::{Ledger, HasConnectionAsync};
use sqlx::{PgPool, postgres::PgPoolOptions};
use std::sync::Arc;
use anyhow::Context;

pub mod constants;
pub mod ledger;
pub mod models;
pub mod processor;
pub mod queries;
pub mod router;
pub mod utils;

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

impl ApiContext {
    pub async fn new(config: Config) -> anyhow::Result<ApiContext> {
        let mainnet_ledger = Ledger::init_async(&config.mainnet_ledger_path)
        .await
        .expect("Error: Could not create mainnet ledger.");

        let devnet_ledger = Ledger::init_async(&config.devnet_ledger_path)
        .await
        .expect("Error: Could not create devnet ledger.");

        let mainnet_db = PgPoolOptions::new()
        .max_connections(25)
        .connect(&config.mainnet_database_url)
        .await
        .context("Error: Could not connect to mainnet database.")?;

        let devnet_db = PgPoolOptions::new()
        .max_connections(25)
        .connect(&config.devnet_database_url)
        .await
        .context("Error: Could not connect to devnet database.")?;

        Ok(ApiContext {
            config: Arc::new(config),
            mainnet_ledger: Arc::new(mainnet_ledger.db),
            devnet_ledger: Arc::new(devnet_ledger.db),
            mainnet_db,
            devnet_db,
        })
    }
}
