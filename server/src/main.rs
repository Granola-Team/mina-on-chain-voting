use std::sync::Arc;
use log::info;
use tower::ServiceBuilder;
use clap::Parser;
use osc_api::{ ApiContext, Config, SubCommand, routes::Build, ledger::Ledger};
use anyhow::Context;
use sqlx::postgres::PgPoolOptions;
use axum::Extension;

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

        let app = router(&config).layer(
        ServiceBuilder::new()
            .layer(Extension(ApiContext {
                config: Arc::new(config),
                ledger: Arc::new(ledger.db),
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
