use anyhow::Context;
use axum::{http::Method, Extension};
use clap::Parser;
use log::info;
use osc_api::{
    ledger::{HasConnectionAsync, Ledger},
    router::Build,
    Config, SubCommand,
};
use sqlx::postgres::PgPoolOptions;
use std::sync::Arc;
use tower::ServiceBuilder;
use tower_http::cors::{Any, CorsLayer};

extern crate dotenv;

// TODO - resume support for Testnet

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    dotenv::dotenv().ok();
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));

    let config = Config::parse();

    match config.subcmd {
        SubCommand::Start => {
            let mainnet_db = PgPoolOptions::new()
                .max_connections(25)
                .connect(&config.database_url)
                .await
                .context("Error: Could not connect to mainnet database.")?;

            let cors = CorsLayer::new()
                .allow_methods([Method::GET, Method::POST])
                .allow_origin(Any);

            let app = router(&config).layer(ServiceBuilder::new().layer(cors).layer(Extension(
                crate::APIContext {
                    config: Arc::new(config),
                    mainnet_db,
                },
            )));

            info!("Axum runtime started.");

            axum::Server::bind(&"0.0.0.0:8080".parse()?)
                .serve(app.into_make_service())
                .await
                .context("Error: Could not start webserver.")
        }
    }
}

fn router(cfg: &Config) -> axum::Router {
    axum::Router::build_v1(cfg)
}
