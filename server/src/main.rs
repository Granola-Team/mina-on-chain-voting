use axum::{http::Method, Extension};
use clap::Parser;
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::signal;
use tower::ServiceBuilder;
use tower_http::{
    cors::{Any, CorsLayer},
    trace::TraceLayer,
};

use crate::config::{Config, Context};
use crate::db::cache::CacheManager;
use crate::db::DBConnectionManager;
use crate::prelude::*;
use crate::router::Build;

mod config;
mod db;
mod error;
mod mina;
mod prelude;
mod router;

extern crate tracing;

pub(crate) const MINA_GOVERNANCE_SERVER: &str = "mina_governance_server";

#[tokio::main]
async fn main() -> Result<()> {
    dotenvy::dotenv().unwrap_or_else(|_| panic!("Error: .env file not found"));
    config::init_tracing();

    let config = Config::parse();
    let cache = CacheManager::build();
    let conn_manager = DBConnectionManager::get_connections(&config);

    let router = axum::Router::build().layer(
        ServiceBuilder::new()
            .layer(TraceLayer::new_for_http())
            .layer(
                CorsLayer::new()
                    .allow_methods([Method::GET, Method::POST, Method::OPTIONS])
                    .allow_origin(Any)
                    .allow_headers(Any),
            )
            .layer(Extension(Context {
                cache: Arc::new(cache),
                conn_manager: Arc::new(conn_manager),
            })),
    );

    tracing::info!(target: MINA_GOVERNANCE_SERVER, "Axum runtime starting...");
    serve(router, config.port).await;
    Ok(())
}

async fn serve(router: axum::Router, port: u16) {
    let addr = SocketAddr::from(([0, 0, 0, 0], port));
    axum::Server::bind(&addr)
        .serve(router.into_make_service())
        .with_graceful_shutdown(shutdown())
        .await
        .expect("Error: failed to start axum runtime");
}

async fn shutdown() {
    let windows = async {
        signal::ctrl_c()
            .await
            .unwrap_or_else(|_| panic!("Error: failed to install windows shutdown handler"));
    };

    #[cfg(unix)]
    let unix = async {
        signal::unix::signal(signal::unix::SignalKind::terminate())
            .unwrap_or_else(|_| panic!("Error: failed to install unix shutdown handler"))
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
