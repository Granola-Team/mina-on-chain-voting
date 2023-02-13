use std::sync::Arc;
use tracing_subscriber::EnvFilter;

use crate::db::cache::CacheManager;
use crate::db::DBConnectionManager;

#[derive(Clone)]
pub(crate) struct Context {
    pub(crate) cache: Arc<CacheManager>,
    pub(crate) conn_manager: Arc<DBConnectionManager>,
}

#[derive(clap::Parser, Clone)]
pub(crate) struct Config {
    /// The connection URL for the application database.
    #[clap(long, env)]
    pub(crate) database_url: String,
    /// The connection URL for the archive database.
    #[clap(long, env)]
    pub(crate) archive_database_url: String,
    /// API Port
    #[clap(long, env, default_value_t = 8080)]
    pub(crate) port: u16,
}

pub(crate) fn init_tracing() {
    tracing_subscriber::fmt::Subscriber::builder()
        .with_env_filter(EnvFilter::new(
            "mina_governance_server=debug,tower_http=debug",
        ))
        .with_writer(std::io::stderr)
        .compact()
        .init();
}
