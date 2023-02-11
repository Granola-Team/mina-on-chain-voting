use axum::{http::Method, Extension};
use clap::Parser;
use router::Build;
use sqlx::{postgres::PgPoolOptions, PgPool};
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::signal;
use tower::ServiceBuilder;
use tower_http::{
    cors::{Any, CorsLayer},
    trace::TraceLayer,
};
use tracing_subscriber::EnvFilter;

use crate::prelude::*;

pub mod error;
pub mod ledger;
pub mod prelude;
pub mod queries;
pub mod router;
pub mod types;

extern crate tracing;

pub const MINA_GOVERNANCE_SERVER: &str = "mina_governance_server";

#[derive(Clone)]
pub struct APIContext {
    pub config: Arc<Config>,
    pub votes_cache: Arc<types::VotesCache>,
    pub ledger_cache: Arc<types::LedgerCache>,
    pub mainnet_db: PgPool,
}

#[derive(clap::Parser, Clone)]
pub struct Config {
    /// The connection URL for the application database.
    #[clap(long, env)]
    pub database_url: String,
    /// The connection URL for the archive database.
    #[clap(long, env)]
    pub archive_database_url: String,
    /// API Port
    #[clap(long, env, default_value_t = 8080)]
    pub port: u16,
}

#[tokio::main]
async fn main() -> Result<()> {
    dotenvy::dotenv().expect("Error: .env file not found.");

    tracing_subscriber::fmt::Subscriber::builder()
        .with_env_filter(EnvFilter::new(
            "mina_governance_server=debug,tower_http=debug",
        ))
        .with_writer(std::io::stderr)
        .compact()
        .init();

    let config = Config::parse();
    let ledger_cache = types::LedgerCache::builder()
        .time_to_live(std::time::Duration::from_secs(60 * 60 * 12))
        .build();

    let votes_cache = types::VotesCache::builder()
        .time_to_live(std::time::Duration::from_secs(60 * 3))
        .build();

    let mainnet_db = PgPoolOptions::new()
        .max_connections(25)
        .connect(&config.database_url)
        .await
        .unwrap();

    let router = axum::Router::build(&config).layer(
        ServiceBuilder::new()
            .layer(TraceLayer::new_for_http())
            .layer(
                CorsLayer::new()
                    .allow_methods([Method::GET, Method::POST, Method::OPTIONS])
                    .allow_origin(Any)
                    .allow_headers(Any)
                    .allow_credentials(true),
            )
            .layer(Extension(APIContext {
                config: Arc::new(config.clone()),
                votes_cache: Arc::new(votes_cache),
                ledger_cache: Arc::new(ledger_cache),
                mainnet_db,
            })),
    );

    tracing::debug!(target: MINA_GOVERNANCE_SERVER, "Axum runtime starting...");
    serve(router, config.port).await;
    Ok(())
}

async fn serve(router: axum::Router, port: u16) {
    let addr = SocketAddr::from(([127, 0, 0, 1], port));
    axum::Server::bind(&addr)
        .serve(router.into_make_service())
        .with_graceful_shutdown(shutdown_signal())
        .await
        .unwrap();
}

async fn shutdown_signal() {
    let windows = async {
        signal::ctrl_c()
            .await
            .expect("Error: Failed to install shutdown handler");
    };

    #[cfg(unix)]
    let unix = async {
        signal::unix::signal(signal::unix::SignalKind::terminate())
            .expect("Error: Failed to install shutdown handler")
            .recv()
            .await;
    };

    #[cfg(not(unix))]
    let terminate = std::future::pending::<()>();

    tokio::select! {
        _ = windows => {},
        _ = unix => {},
    }

    println!("Signal received - starting graceful shutdown...");
}
