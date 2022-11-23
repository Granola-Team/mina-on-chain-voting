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
        AxumQuery(mut params): AxumQuery<HashMap<String, QueryRequestFilter>>,
        ctx: Extension<crate::ApiContext>,
        ) -> impl IntoResponse {
    let network_opt = params.remove("network");

    if let Some(network) = network_opt {
        let signals = ctx
            .get_signals(&network)
            .await
            .expect(&format!("Could not get signals on {:?}!", &network));

        let latest_block_height = ctx.get_latest_block_height(&network)
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