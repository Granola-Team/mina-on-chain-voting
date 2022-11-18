pub mod processor;

use super::Config;
use axum::{
    extract::{Path, Query as AxumQuery},
    http::StatusCode,
    response::IntoResponse,
    routing::{get, get_service},
    Extension, Router,
};
use axum_extra::routing::SpaRouter;
use tower_http::services::ServeFile;

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::queries;

use processor::SignalProcessor;

pub trait Build {
    fn build_v1(cfg: &Config) -> Router;
}

impl Build for Router {
    fn build_v1(cfg: &Config) -> Router {
        let spa = SpaRouter::new("/assets", format!("{}/assets", &cfg.client_path))
            .index_file("index.html");

        let react_router_fallback =
            get_service(ServeFile::new(format!("{}/index.html", &cfg.client_path))).handle_error(
                |error: std::io::Error| async move {
                    (
                        StatusCode::INTERNAL_SERVER_ERROR,
                        format!("Internal Server Error: {}", error),
                    )
                },
            );

        Router::new()
            .merge(spa)
            .route("/api/v1/:keyword", get(handler))
            .fallback(react_router_fallback)
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
