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

use std::collections::HashMap;

use graphql_client::{reqwest::post_graphql_blocking as post_graphql, GraphQLQuery, Response};
use reqwest::blocking::Client;

use transaction_query::TransactionQueryTransactions;

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

type DateTime = String;

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "src/database/transaction_schema.graphql",
    query_path = "src/database/transaction_query.graphql",
    response_derives = "Debug"
)]
pub struct TransactionQuery;

#[allow(dead_code, clippy::unwrap_used, clippy::upper_case_acronyms)]
pub(crate) fn fetch_transactions_graphql() {
    let variables = transaction_query::Variables {
        date_time_gte: Some("2023-05-20T06:00:00Z".to_string()),
        date_time_lte: Some("2023-05-28T06:00:00Z".to_string()),
        memo1: Some("E4YVPwLUR2LrP9tSSi3fjw1svcZys1gJHrGvRefwVTCMbP2NQRqdW".to_string()),
        memo2: Some("E4YVe5wRALCJJ2dGEwRMyH7Z8y1QxzM76M8rXivFo5XbeBJdKryV6".to_string()),
        memo3: Some("E4YbUmaZjNgLgezBD3JzyGKuCn4iugZ5EcXT1JuNTudm5tT4MHvKz".to_string()),
        memo4: Some("E4YbUmaZZqAoUdTZYvZkSmLjHfccTMbb5RnTQHixwRWq2YqLdLZyE".to_string()),
    };
    let client = Client::new();
    let response_body: Response<transaction_query::ResponseData> =
        post_graphql::<TransactionQuery, _>(&client, "https://graphql.minaexplorer.com", variables)
            .unwrap();

    let txns = response_body.data.unwrap().transactions;

    let txns = txns
        .into_iter()
        .flatten()
        .filter(|tx| tx.to.clone().unwrap() == tx.from.clone().unwrap())
        .fold(HashMap::new(), |mut map, txn| {
            // Dedup based on from public key keeping the most recent transaction
            map.entry(txn.from.clone().unwrap()).or_insert(txn);
            map
        })
        .into_values()
        .collect::<Vec<TransactionQueryTransactions>>();
    println!("{:?}", txns.len());
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
