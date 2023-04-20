use anyhow::Context;
use axum::extract::Path;
use axum::http::StatusCode;
use axum::response::IntoResponse;
use axum::routing::get;
use axum::{Extension, Json, Router};
use diesel::prelude::*;
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

use crate::database::archive::fetch_chain_tip;
use crate::database::archive::fetch_transactions;
use crate::models::diesel::MinaProposal;
use crate::models::ledger::Ledger;
use crate::models::vote::MinaVote;
use crate::models::vote::MinaVoteWithWeight;
use crate::prelude::*;

pub(crate) fn router() -> Router {
    Router::new()
        .route("/api/proposals", get(get_mina_proposals))
        .route("/api/proposal/:id", get(get_mina_proposal))
        .route("/api/proposal/:id/results", get(get_mina_proposal_result))
}

#[allow(clippy::unused_async)]
async fn get_mina_proposals(ctx: Extension<crate::Context>) -> Result<impl IntoResponse> {
    use crate::schema::mina_proposals::dsl as mina_proposal_dsl;

    let conn = &mut ctx
        .conn_manager
        .main
        .get()
        .context("failed to get primary db connection")?;

    let proposals: Vec<MinaProposal> = mina_proposal_dsl::mina_proposals
        .order(mina_proposal_dsl::id.desc())
        .load(conn)?;

    Ok((StatusCode::OK, Json(proposals)).into_response())
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

    let conn = &mut ctx
        .conn_manager
        .main
        .get()
        .context("failed to get primary db connection")?;

    let proposal: MinaProposal = mina_proposal_dsl::mina_proposals.find(id).first(conn)?;

    if let Some(cached) = ctx.cache.votes.get(&proposal.key) {
        let response = GetMinaProposalResponse {
            proposal,
            votes: cached.to_vec(),
        };

        return Ok((StatusCode::OK, Json(response)).into_response());
    }

    let transactions =
        fetch_transactions(&ctx.conn_manager, proposal.start_time, proposal.end_time)?;

    let chain_tip = fetch_chain_tip(&ctx.conn_manager)?;

    let votes = Wrapper(
        transactions
            .into_iter()
            .map(std::convert::Into::into)
            .collect(),
    )
    .process(&proposal.key, chain_tip)
    .sort_by_timestamp()
    .to_vec()
    .0;

    ctx.cache
        .votes
        .insert(proposal.key.clone(), Arc::new(votes.clone()))
        .await;

    let response = GetMinaProposalResponse { proposal, votes };

    Ok((StatusCode::OK, Json(response)).into_response())
}

#[derive(Serialize, Deserialize)]
struct GetMinaProposalResultResponse {
    #[serde(flatten)]
    proposal: MinaProposal,
    total_stake_weight: Decimal,
    positive_stake_weight: Decimal,
    negative_stake_weight: Decimal,
    votes: Vec<MinaVoteWithWeight>,
}

async fn get_mina_proposal_result(
    ctx: Extension<crate::Context>,
    Path(id): Path<i32>,
) -> Result<impl IntoResponse> {
    use crate::schema::mina_proposals::dsl as mina_proposal_dsl;

    let conn = &mut ctx
        .conn_manager
        .main
        .get()
        .context("failed to get primary db connection")?;

    let proposal: MinaProposal = mina_proposal_dsl::mina_proposals.find(id).first(conn)?;

    if proposal.ledger_hash.is_none() {
        let response = GetMinaProposalResultResponse {
            proposal,
            total_stake_weight: Decimal::ZERO,
            positive_stake_weight: Decimal::ZERO,
            negative_stake_weight: Decimal::ZERO,
            votes: Vec::new(),
        };
        return Ok((StatusCode::OK, Json(response)).into_response());
    }
    let hash = proposal
        .ledger_hash
        .clone()
        .expect("hash should always be present");

    let ledger = if let Some(cached_ledger) = ctx.cache.ledger.get(&hash) {
        Ledger(cached_ledger.to_vec())
    } else {
        let ledger = Ledger::fetch(&hash, ctx.network, &ctx.ledger_storage_path).await?;

        ctx.cache
            .ledger
            .insert(hash, Arc::new(ledger.0.clone()))
            .await;

        ledger
    };

    let votes = if let Some(cached_votes) = ctx.cache.votes_weighted.get(&proposal.key) {
        cached_votes.to_vec()
    } else {
        let transactions =
            fetch_transactions(&ctx.conn_manager, proposal.start_time, proposal.end_time)?;

        let chain_tip = fetch_chain_tip(&ctx.conn_manager)?;

        let votes = Wrapper(
            transactions
                .into_iter()
                .map(std::convert::Into::into)
                .collect(),
        )
        .into_weighted(&proposal, &ledger, chain_tip)
        .sort_by_timestamp()
        .0;

        ctx.cache
            .votes_weighted
            .insert(proposal.key.clone(), Arc::new(votes.clone()))
            .await;

        votes
    };

    let mut positive_stake_weight = Decimal::from(0);
    let mut negative_stake_weight = Decimal::from(0);

    for vote in &votes {
        if vote.memo.split_whitespace().next().eq(&Some("no")) {
            negative_stake_weight += vote.weight;
        } else {
            positive_stake_weight += vote.weight;
        }
    }

    let response = GetMinaProposalResultResponse {
        proposal,
        total_stake_weight: positive_stake_weight + negative_stake_weight,
        positive_stake_weight,
        negative_stake_weight,
        votes,
    };

    Ok((StatusCode::OK, Json(response)).into_response())
}
