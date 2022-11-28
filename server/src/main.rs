use anyhow::Context;
use axum::{http::Method, Extension};
use clap::Parser;
use log::info;
use osc_api::{
    ledger::{HasConnectionAsync, Ledger},
    router::Build,
    ApiContext, Config, SubCommand, queries,
};
use sqlx::postgres::PgPoolOptions;
use std::sync::Arc;
use tower::ServiceBuilder;
use tower_http::cors::{Any, CorsLayer};

extern crate dotenv;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    dotenv::dotenv().ok();
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));

    let config = Config::parse();

    match config.subcmd {
        SubCommand::Start => {
            let api_context = ApiContext::new(config).await?;
            let app = queries::build_router(api_context).await;

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
