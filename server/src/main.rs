use anyhow::Context;
use axum::{http::{Method, HeaderValue}, Extension};
use clap::Parser;
use osc_api::{router::Build, Config};
use sqlx::postgres::PgPoolOptions;
use std::sync::Arc;
use tower::ServiceBuilder;
use tower_http::cors::{CorsLayer, Any};

extern crate dotenv;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    dotenv::dotenv().ok();
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));

    let config = Config::parse();
    let ledger_cache = osc_api::ledger::LedgerCache::builder()
        .time_to_live(std::time::Duration::from_secs(60 * 60 * 12))
        .build();

    let signal_cache = osc_api::signal::SignalCache::builder()
        .time_to_live(std::time::Duration::from_secs(60 * 3))
        .build();

    let mainnet_db = PgPoolOptions::new()
        .max_connections(25)
        .connect(&config.database_url)
        .await
        .context("Error: Could not connect to mainnet database.")?;

    let allowed_origins = match std::env::var("ALLOWED_ORIGINS") {
        Ok(val) => {
            let origin = val.split(",").map(|s| s.trim()).collect::<Vec<&str>>().join(",");
            HeaderValue::from_str(&origin).unwrap()
        },
        Err(_) => return Err("Origin not allowed"),
    };

    let cors = CorsLayer::new()
        .allow_methods([Method::GET, Method::POST, Method::OPTIONS])
        .allow_origin(allowed_origins)
        .allow_headers(Any)
        .allow_credentials(true);

    let app = router(&config).layer(ServiceBuilder::new().layer(cors).layer(Extension(
        osc_api::APIContext {
            config: Arc::new(config),
            signal_cache: Arc::new(signal_cache),
            ledger_cache: Arc::new(ledger_cache),
            mainnet_db,
        },
    )));

    log::info!("Axum runtime started.");

    axum::Server::bind(&"0.0.0.0:8080".parse()?)
        .serve(app.into_make_service())
        .await
        .context("Error: Could not start webserver.")
}

fn router(cfg: &Config) -> axum::Router {
    axum::Router::build_v1(cfg)
}
