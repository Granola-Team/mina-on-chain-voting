use osc_api::{ ApiContext, Config, SubCommand, routes::Build, ledger::Ledger, queries};
use tower_http::cors::{Any, CorsLayer};
use axum::{Extension, http::Method};
use sqlx::postgres::PgPoolOptions;
use tower::ServiceBuilder;
use anyhow::Context;
use std::sync::Arc;
use clap::Parser;
use log::info;

extern crate dotenv;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    dotenv::dotenv().ok();
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));

    let config = Config::parse();

    match config.subcmd {
        SubCommand::Start => {
        let ledger = Ledger::init().await.expect("Error: Could not create ledger.");

        let db = PgPoolOptions::new()
        .max_connections(50)
        .connect(&config.database_url)
        .await
        .context("Error: Could not connect to database.")?;

        // Gets all available signals in DB.
        // Once archive node is activally populating DB -> change this to get signals on request.
        let signals = queries::get_signals(&db).await.expect("Error: Could not get signals.");

        let cors = CorsLayer::new()
        .allow_methods([Method::GET, Method::POST])
        .allow_origin(Any);

        let app = router(&config).layer(
        ServiceBuilder::new()
            .layer(cors)
            .layer(Extension(ApiContext {
                config: Arc::new(config),
                ledger: Arc::new(ledger.db),
                signals: Arc::new(signals),
                db,
            }))
         );

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
