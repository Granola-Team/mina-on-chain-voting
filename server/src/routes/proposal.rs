use axum::{
    extract::Path, http::StatusCode, response::IntoResponse, routing::get, Extension, Json, Router,
};
use diesel::prelude::*;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

use crate::config::NetworkConfig;
use crate::database::archive::{fetch_chain_tip, fetch_transactions};
use crate::models::diesel::MinaProposal;
use crate::models::ledger::Ledger;
use crate::models::vote::MinaVote;
use crate::prelude::*;

pub(crate) fn router() -> Router {
    Router::new()
        .route("/api/proposal/:id", get(get_mina_proposal))
        .route("/api/proposal/:id/results", get(get_mina_proposal_result))
}

#[derive(Serialize, Deserialize)]
struct GetMinaProposalResponse {
    #[serde(flatten)]
    proposal: MinaProposal,
    votes: Vec<MinaVote>,
}

async fn get_mina_proposal(
    ctx: Extension<crate::Context>,
    Path(id): Path<i32>,
) -> Result<impl IntoResponse> {
    use crate::schema::mina_proposals::dsl as mina_proposal_dsl;

    let conn = &mut ctx.conn_manager.main.get()?;
    let proposal: MinaProposal = mina_proposal_dsl::mina_proposals.find(id).first(conn)?;

    if let Some(cached) = ctx.cache.votes.get(&proposal.key) {
        let response = GetMinaProposalResponse {
            proposal,
            votes: cached.to_vec(),
        };

        return Ok((StatusCode::OK, Json(response)).into_response());
    }

    let transactions = fetch_transactions(
        &ctx.conn_manager,
        proposal.global_start_slot,
        proposal.global_end_slot,
    )?;

    let chain_tip = fetch_chain_tip(&ctx.conn_manager)?;

    let votes = W(transactions
        .into_iter()
        .map(std::convert::Into::into)
        .collect())
    .process(&proposal.key, chain_tip)
    .sort_by_timestamp()
    .inner();

    ctx.cache
        .votes
        .insert(proposal.key.clone(), Arc::new(votes.clone()))
        .await;

    let response = GetMinaProposalResponse { proposal, votes };

    Ok((StatusCode::OK, Json(response)).into_response())
}

#[derive(Serialize, Deserialize)]
struct GetMinaProposalResultsResponse {
    #[serde(flatten)]
    proposal: MinaProposal,
    votes: Vec<MinaVote>,
}

async fn get_mina_proposal_result(
    ctx: Extension<crate::Context>,
    Path(id): Path<i32>,
) -> Result<impl IntoResponse> {
    use crate::schema::mina_proposals::dsl as mina_proposal_dsl;

    let conn = &mut ctx.conn_manager.main.get()?;
    let proposal: MinaProposal = mina_proposal_dsl::mina_proposals.find(id).first(conn)?;

    let hash = proposal
        .ledger_hash
        .clone()
        .ok_or(Error::Ledger(f!("Ledger for proposal {id} not found")))?;

    let ledger = if let Some(cached_ledger) = ctx.cache.ledger.get(&hash) {
        Ledger(cached_ledger.to_vec())
    } else {
        let network = NetworkConfig::Mainnet;
        let ledger = Ledger::fetch(&hash, network).await?;

        ctx.cache
            .ledger
            .insert(hash, Arc::new(ledger.0.clone()))
            .await;

        ledger
    };

    let votes = if let Some(cached_votes) = ctx.cache.votes.get(&f!("r-{}", proposal.key)) {
        cached_votes.to_vec()
    } else {
        let transactions = fetch_transactions(
            &ctx.conn_manager,
            proposal.global_start_slot,
            proposal.global_end_slot,
        )?;

        let chain_tip = fetch_chain_tip(&ctx.conn_manager)?;

        let votes = W(transactions
            .into_iter()
            .map(std::convert::Into::into)
            .collect())
        .process_weighted(&proposal.key, &ledger, chain_tip)
        .sort_by_timestamp()
        .inner();

        ctx.cache
            .votes
            .insert(f!("r-{}", proposal.key), Arc::new(votes.clone()))
            .await;

        votes
    };

    let response = GetMinaProposalResponse { proposal, votes };

    Ok((StatusCode::OK, Json(response)).into_response())
}
