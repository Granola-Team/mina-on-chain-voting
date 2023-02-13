use axum::{http::Method, Extension};
use clap::Parser;
use std::sync::Arc;
use tower::ServiceBuilder;
use tower_http::{
    cors::{Any, CorsLayer},
    trace::TraceLayer,
};

use crate::config::*;
use crate::db::cache::*;
use crate::db::conn::*;
use crate::http::serve;
use crate::http::Build;
use crate::prelude::*;

mod chain;
mod config;
mod db;
mod error;
mod http;
mod prelude;
mod types;

extern crate tracing;

pub(crate) const MINA_GOVERNANCE_SERVER: &str = "mina_governance_server";

#[tokio::main]
async fn main() -> Result<()> {
    dotenvy::dotenv().unwrap_or_else(|_| panic!("Error: .env file not found."));
    config::init_tracing().unwrap_or_else(|_| panic!("Error: failed to initialize tracing."));

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

    tracing::info!(target: MINA_GOVERNANCE_SERVER, "Axum runtime starting");
    serve(router, config.port).await;
    Ok(())
}
