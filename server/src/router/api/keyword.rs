use core::fmt;
use std::str::FromStr;

use axum::{
    extract::{Path, Query as AxumQuery},
    http::StatusCode,
    response::IntoResponse,
    Extension,
};
use serde::{de, Deserialize, Deserializer};

use crate::{queries, types::Network};

use crate::processor::SignalProcessor;

#[derive(Debug, Deserialize)]
pub struct QueryParams {
    #[serde(default, deserialize_with = "empty_string_as_none")]
    timestamp: Option<i64>,
    network: Option<Network>,
}

fn empty_string_as_none<'de, D, T>(de: D) -> Result<Option<T>, D::Error>
where
    D: Deserializer<'de>,
    T: FromStr,
    T::Err: fmt::Display,
{
    let opt = Option::<String>::deserialize(de)?;
    match opt.as_deref() {
        None | Some("") => Ok(None),
        Some(s) => FromStr::from_str(s).map_err(de::Error::custom).map(Some),
    }
}

pub async fn handler(
    Path(key): Path<String>,
    AxumQuery(params): AxumQuery<QueryParams>,
    ctx: Extension<crate::APIContext>,
) -> impl IntoResponse {
    let network_opt = params.network;
    let timestamp_opt = params.timestamp;

    if let Some(network) = network_opt {
        let signals = match network {
            Network::Mainnet => queries::get_signals(&ctx.mainnet_db, timestamp_opt)
                .await
                .expect("Error: Could not get mainnet signals."),
        };

        let latest_block_height = queries::get_latest_blockheight(&ctx, &network)
            .await
            .expect("Error: Could not get latest block.");

        let response_entity = match network {
            Network::Mainnet => {
                ctx.mainnet_ledger
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
