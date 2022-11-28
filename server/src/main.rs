use anyhow::Context;
use clap::Parser;
use log::info;
use osc_api::{queries, ApiContext, Config, SubCommand};

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
