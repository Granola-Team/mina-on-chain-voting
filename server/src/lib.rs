use sqlx::PgPool;
use std::sync::Arc;

pub mod ledger;
pub mod queries;
pub mod router;
pub mod signal;
pub mod types;
pub mod utils;

#[derive(Clone)]
pub struct APIContext {
    pub config: Arc<Config>,
    pub signal_cache: Arc<crate::signal::SignalCache>,
    pub ledger_cache: Arc<crate::ledger::LedgerCache>,
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
}
