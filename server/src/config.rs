use axum::http::HeaderValue;
use axum::http::Method;
use std::collections::HashSet;
use std::sync::Arc;
use tower_http::cors::Any;
use tower_http::cors::CorsLayer;
use tracing_subscriber::EnvFilter;

use crate::db::cache::CacheManager;
use crate::db::DBConnectionManager;
use crate::prelude::*;

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
    /// API Port.
    #[clap(long, env, default_value_t = 8080)]
    pub(crate) port: u16,
    /// Origins allowed to make cross-site requests.
    #[clap(long, env = "SERVER_ALLOWED_ORIGINS", value_parser = parse_allowed_origins )]
    pub(crate) allowed_origins: HashSet<String>,
}

fn parse_allowed_origins(arg: &str) -> Result<HashSet<String>> {
    let allowed_origins = HashSet::from_iter(
        arg.split_whitespace()
            .map(std::borrow::ToOwned::to_owned)
            .into_iter()
            .collect::<HashSet<_>>(),
    );

    if allowed_origins.is_empty() {
        return Err(Error::Config(f!(
            "failed to parse allowed_origins: {allowed_origins:?}"
        )));
    }

    Ok(allowed_origins)
}

pub(crate) fn init_cors(cfg: &Config) -> CorsLayer {
    let origins = cfg
        .allowed_origins
        .clone()
        .into_iter()
        .map(|origin| {
            origin
                .parse()
                .unwrap_or_else(|_| panic!("Error: failed parsing allowed-origin {origin}"))
        })
        .collect::<Vec<HeaderValue>>();

    let layer = if cfg.allowed_origins.contains("*") {
        CorsLayer::new().allow_origin(Any)
    } else {
        CorsLayer::new().allow_origin(origins)
    };

    layer
        .allow_methods([Method::GET, Method::POST, Method::OPTIONS])
        .allow_headers(Any)
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
