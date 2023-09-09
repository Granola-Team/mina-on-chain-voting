use axum::{http::StatusCode, response::IntoResponse, routing::get, Extension, Json, Router};
use serde::{Deserialize, Serialize};

use crate::prelude::*;

pub(crate) fn router() -> Router {
    Router::new().route("/api/info", get(get_core_api_info))
}

#[derive(Serialize, Deserialize)]
struct GetCoreApiInfoResponse {
    chain_tip: i64,
    current_slot: i64,
}

#[allow(clippy::unused_async)]
async fn get_core_api_info(ctx: Extension<crate::Context>) -> Result<impl IntoResponse> {
    let chain_tip = ctx.conn_manager.fetch_chain_tip()?;
    let current_slot = ctx.conn_manager.fetch_latest_slot()?;

    let response = GetCoreApiInfoResponse {
        chain_tip,
        current_slot,
    };

    Ok((StatusCode::OK, Json(response)).into_response())
}
