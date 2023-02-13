use axum::{
    extract::{Path, Query as AxumQuery},
    http::StatusCode,
    response::IntoResponse,
    routing::get,
    Extension, Router,
};
use serde::{Deserialize, Serialize};

use crate::db::queries::{fetch_chain_tip, fetch_votes};
use crate::mina::ledger::get_ledger;
use crate::prelude::*;

pub(crate) fn router() -> Router {
    Router::new()
        .route("/api/:keyword", get(keyword_handler))
        .route("/api/:keyword/results", get(keyword_results_handler))
}

#[derive(Debug, Serialize, Deserialize)]
struct KeywordParams {
    start: i64,
    end: i64,
}

async fn keyword_handler(
    Path(key): Path<String>,
    AxumQuery(params): AxumQuery<KeywordParams>,
    ctx: Extension<crate::Context>,
) -> impl IntoResponse {
    let votes = fetch_votes(&ctx.conn_manager, &ctx.cache, params.start, params.end).await;
    let chain_tip = fetch_chain_tip(&ctx.conn_manager);

    if let (Ok(votes), Ok(chain_tip)) = (votes, chain_tip) {
        let votes = W(votes).process(key, chain_tip);
        let sorted_votes = votes.sort_by_timestamp().0;

        return (StatusCode::OK, axum::Json(sorted_votes)).into_response();
    }

    (StatusCode::INTERNAL_SERVER_ERROR).into_response()
}

#[derive(Debug, Serialize, Deserialize)]
struct KeywordResultsParams {
    start: i64,
    end: i64,
    hash: String,
}

async fn keyword_results_handler(
    Path(key): Path<String>,
    AxumQuery(params): AxumQuery<KeywordResultsParams>,
    ctx: Extension<crate::Context>,
) -> impl IntoResponse {
    let votes = fetch_votes(&ctx.conn_manager, &ctx.cache, params.start, params.end).await;
    let chain_tip = fetch_chain_tip(&ctx.conn_manager);

    if let (Ok(votes), Ok(chain_tip)) = (votes, chain_tip) {
        let ledger = get_ledger(params.hash, &ctx.cache).await;

        if let Ok(ledger) = ledger {
            let votes = W(votes);
            let votes_weighted = votes.process_weighted(key, &ledger, chain_tip);

            return (
                StatusCode::OK,
                axum::Json(votes_weighted.sort_by_timestamp().0),
            )
                .into_response();
        }
    }

    (StatusCode::INTERNAL_SERVER_ERROR).into_response()
}
