use axum::{
    extract::{Path, Query},
    http::StatusCode,
    response::IntoResponse,
    routing::get,
    Extension, Json, Router,
};
use diesel::prelude::*;
use serde::{Deserialize, Serialize};

use crate::db::queries::{fetch_chain_tip, fetch_votes};
use crate::mina::ledger::get_ledger;
use crate::prelude::*;

pub(crate) fn router() -> Router {
    Router::new()
        .route("/api/proposal/:key", get(get_mina_proposal))
        .route("/api/proposal/:key/results", get(keyword_results_handler))
}

async fn get_mina_proposal(
    ctx: Extension<crate::Context>,
    Path(key): Path<String>,
) -> impl IntoResponse {
    use crate::schema::mina_proposals::dsl as mina_proposal_dsl;

    let votes = fetch_votes(&ctx.conn_manager, &ctx.cache, 1, 1).await;

    let conn = &mut ctx.conn_manager.main.get().unwrap();

    let _proposal = mina_proposal_dsl::mina_proposals
        .select(mina_proposal_dsl::key)
        .filter(mina_proposal_dsl::key.eq(&key))
        .load::<String>(conn);

    if let Ok(votes) = votes {
        if let Ok(chain_tip) = fetch_chain_tip(&ctx.conn_manager) {
            let votes = W(votes).process(key, chain_tip);
            let sorted_votes = votes.sort_by_timestamp().0;

            return (StatusCode::OK, Json(sorted_votes)).into_response();
        }
    }

    (StatusCode::INTERNAL_SERVER_ERROR).into_response()
}

#[derive(Debug, Serialize, Deserialize)]
struct GetKeywordResultsParams {
    start: i64,
    end: i64,
    hash: String,
}

async fn keyword_results_handler(
    Path(key): Path<String>,
    Query(params): Query<GetKeywordResultsParams>,
    ctx: Extension<crate::Context>,
) -> impl IntoResponse {
    let votes = fetch_votes(&ctx.conn_manager, &ctx.cache, params.start, params.end).await;
    let chain_tip = fetch_chain_tip(&ctx.conn_manager);

    if let (Ok(votes), Ok(chain_tip)) = (votes, chain_tip) {
        let ledger = get_ledger(params.hash, &ctx.cache).await;

        if let Ok(ledger) = ledger {
            let votes = W(votes);
            let votes_weighted = votes.process_weighted(key, &ledger, chain_tip);

            return (StatusCode::OK, Json(votes_weighted.sort_by_timestamp().0)).into_response();
        }
    }

    (StatusCode::INTERNAL_SERVER_ERROR).into_response()
}
