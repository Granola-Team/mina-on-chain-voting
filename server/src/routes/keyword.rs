use axum::{
    extract::{Path, Query as AxumQuery},
    http::StatusCode,
    response::IntoResponse,
    Extension,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::{
    models::{DBResponse, ResponseEntity},
    queries,
};

use super::processor::SignalProcessor;

pub async fn parse_responses(
    key: String,
    latest_block: i64,
    signals: Vec<DBResponse>,
    ctx: Extension<crate::ApiContext>,
    network: QueryRequestFilter,
) -> ResponseEntity {
    match network {
        QueryRequestFilter::Mainnet => {
            ctx.mainnet_ledger
                .call(move |conn| SignalProcessor::new(Box::new(conn), &key, latest_block, signals).run())
                .await
        }
        QueryRequestFilter::Devnet => {
            ctx.devnet_ledger
                .call(move |conn| SignalProcessor::new(Box::new(conn), &key, latest_block, signals).run())
                .await
        }
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum QueryRequestFilter {
    Mainnet,
    Devnet,
}

pub async fn handler(
    Path(key): Path<String>,
    AxumQuery(mut params): AxumQuery<HashMap<String, QueryRequestFilter>>,
    ctx: Extension<crate::ApiContext>,
) -> impl IntoResponse {
    let network_opt = params.remove("network");

    if let Some(network) = network_opt {
        let signals = match network {
            QueryRequestFilter::Mainnet => queries::get_signals(&ctx.mainnet_db)
                .await
                .expect("Error: Could not get mainnet signals."),
            QueryRequestFilter::Devnet => queries::get_signals(&ctx.devnet_db)
                .await
                .expect("Error: Could not get devnet signals."),
        };

        let latest_block_height = queries::get_latest_blockheight(&ctx, &network)
            .await
            .expect("Error: Could not get latest block.");
        let result = parse_responses(key, latest_block_height, signals, ctx, network).await;

        return (StatusCode::ACCEPTED, axum::Json(result)).into_response();
    }

    (
        StatusCode::BAD_REQUEST,
        axum::Json("Error: Network param not provided."),
    )
        .into_response()
}
