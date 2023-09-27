use axum::Extension;
use clap::Parser;
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::signal;
use tower::ServiceBuilder;
use tower_http::trace::TraceLayer;

use crate::config::Config;
use crate::config::Context;
use crate::database::cache::CacheManager;
use crate::database::DBConnectionManager;
use crate::prelude::*;
use crate::routes::Build;

mod config;
mod database;
mod error;
mod models;
mod prelude;
mod routes;
mod schema;

extern crate tracing;

pub(crate) const MINA_GOVERNANCE_SERVER: &str = "mina_governance_server";

fn fetch_url_and_print_response() -> Result<(), reqwest::Error> {
    let url = "https://raw.githubusercontent.com/Granola-Team/mina-ledger/main/{network}/{hash}.json";

    let response = reqwest::blocking::get(url)?;

    if response.status().is_success() {
        let body = response.text()?;
        println!("Response:\n{body}");
    } else {
        println!("Error: Status Code {:?}", response.status());
    }

    Ok(())
}

#[tokio::main]
async fn main() -> Result<()> {
    dotenvy::dotenv().ok();
    config::init_tracing();

    let config = Config::parse();
    let cache = CacheManager::build();
    let cors = config::init_cors(&config);

    tracing::info!(
        target: MINA_GOVERNANCE_SERVER,
        "Initializing database connection pools..."
    );

    // Spawn the URL fetching function concurrently
    let fetch_task = tokio::spawn(async move {
        if let Err(err) = fetch_url_and_print_response() {
            // Handle any errors here
            eprintln!("Error: {err:?}");
        }
    });

    let conn_manager = DBConnectionManager::get_connections(&config);

    let router = axum::Router::build().layer(
        ServiceBuilder::new()
            .layer(TraceLayer::new_for_http())
            .layer(cors)
            .layer(Extension(Context {
                cache: Arc::new(cache),
                conn_manager: Arc::new(conn_manager),
                network: config.mina_network,
                ledger_storage_path: config.ledger_storage_path,
            })),
    );

    serve(router.clone(), config.port).await;

    // Wait for the fetch task to complete before exiting
    let _ = fetch_task.await;

    Ok(())
}

async fn serve(router: axum::Router, port: u16) {
    let addr = SocketAddr::from(([0, 0, 0, 0], port));

    tracing::info!(
        target: MINA_GOVERNANCE_SERVER,
        "Started server on {addr} - http://{addr}"
    );

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
