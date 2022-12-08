use std::collections::HashMap;

use axum::{
    extract::{Path, Query as AxumQuery},
    http::StatusCode,
    response::IntoResponse,
    Extension,
};

use crate::{queries, router::QueryRequestFilter};

use crate::processor::SignalProcessor;

pub async fn handler(
        Path(key): Path<String>,
        AxumQuery(mut network_params): AxumQuery<HashMap<String, QueryRequestFilter>>,
        AxumQuery(mut timestamp_params): AxumQuery<HashMap<String, i64>>,
        ctx: Extension<crate::ApiContext>,
        ) -> impl IntoResponse {
    let network_opt = network_params.remove("network");
    let timestamp_opt = timestamp_params.remove("timestamp");

    if let Some(network) = network_opt {
        let signals = match network {
            QueryRequestFilter::Mainnet => queries::get_signals(&ctx.mainnet_db, timestamp_opt)
            .await
            .expect("Error: Could not get mainnet signals."),
            QueryRequestFilter::Devnet => queries::get_signals(&ctx.devnet_db, timestamp_opt)
            .await
            .expect("Error: Could not get devnet signals."),
        };

        let latest_block_height = queries::get_latest_blockheight(&ctx, &network)
        .await
        .expect("Error: Could not get latest block.");

        let response_entity = match network {
            QueryRequestFilter::Mainnet => {
                ctx.mainnet_ledger
                .call(move |conn| {
                    SignalProcessor::new(conn, &key, latest_block_height, signals)
                    .run()
                    .sort()
                })
                .await
            }
            QueryRequestFilter::Devnet => {
                ctx.devnet_ledger
                .call(move |conn| {
                    SignalProcessor::new(conn, &key, latest_block_height, signals)
                    .run()
                    .sort()
                })
                .await
            }
        };

        return (StatusCode::ACCEPTED, axum::Json(response_entity)).into_response();
    }

    (
            StatusCode::BAD_REQUEST,
    axum::Json("Error: Network param not provided."),
    )
    .into_response()
}