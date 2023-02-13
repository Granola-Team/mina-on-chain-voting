use axum::{
    extract::{Path, Query as AxumQuery},
    http::StatusCode,
    response::IntoResponse,
    routing::get,
    Extension, Router,
};
use base58check::FromBase58Check;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::db::queries::*;
use crate::mina::ledger::*;
use crate::prelude::*;
use crate::types::{SortByTimestamp, Vote, VoteExt, VoteWithWeight};

pub fn router() -> Router {
    Router::new()
        .route("/api/:keyword", get(keyword_handler))
        .route("/api/:keyword/results", get(keyword_results_handler))
}

#[derive(Debug, Serialize, Deserialize)]
pub struct KeywordParams {
    start: i64,
    end: i64,
}

pub(crate) async fn keyword_handler(
    Path(key): Path<String>,
    AxumQuery(params): AxumQuery<KeywordParams>,
    ctx: Extension<crate::Context>,
) -> impl IntoResponse {
    let votes = fetch_votes(&ctx.conn_manager, &ctx.cache, params.start, params.end).await;
    let chain_tip = fetch_chain_tip(&ctx.conn_manager).await;

    if let (Ok(votes), Ok(chain_tip)) = (votes, chain_tip) {
        let votes = process_votes(key, votes, chain_tip);
        return (StatusCode::OK, axum::Json(votes.sort_by_timestamp())).into_response();
    }

    return (StatusCode::INTERNAL_SERVER_ERROR).into_response();
}

fn process_votes(key: impl Into<String>, votes: Vec<Vote>, chain_tip: i64) -> Vec<Vote> {
    let mut map = HashMap::new();
    let key = key.into();

    for mut vote in votes {
        if let Some(memo) = decode_memo(&key, &vote.memo) {
            if match_memo(&key, &memo) {
                vote.update_memo(memo);

                if chain_tip - vote.height >= 10 {
                    vote.mark_canonical();
                }

                if let Some(current_vote) = map.get_mut(&vote.account) {
                    if is_newer(&vote, current_vote) {
                        *current_vote = vote;
                    }
                } else {
                    map.insert(vote.account.clone(), vote);
                }
            }
        }
    }
    map.values().cloned().collect()
}

#[derive(Debug, Serialize, Deserialize)]
pub struct KeywordResultsParams {
    start: i64,
    end: i64,
    hash: String,
}

pub(crate) async fn keyword_results_handler(
    Path(key): Path<String>,
    AxumQuery(params): AxumQuery<KeywordResultsParams>,
    ctx: Extension<crate::Context>,
) -> impl IntoResponse {
    let votes = fetch_votes(&ctx.conn_manager, &ctx.cache, params.start, params.end).await;
    let chain_tip = fetch_chain_tip(&ctx.conn_manager).await;

    if let (Ok(votes), Ok(chain_tip)) = (votes, chain_tip) {
        let ledger = get_ledger(params.hash, &ctx.cache).await;
        if let Ok(ledger) = ledger {
            let votes_with_stake = process_voting_results(key, votes, chain_tip, ledger);
            if let Ok(votes_with_stake) = votes_with_stake {
                return (
                    StatusCode::OK,
                    axum::Json(votes_with_stake.sort_by_timestamp()),
                )
                    .into_response();
            }
        }
    }

    return (StatusCode::INTERNAL_SERVER_ERROR).into_response();
}

fn process_voting_results(
    key: impl Into<String>,
    votes: Vec<Vote>,
    chain_tip: i64,
    ledger: Vec<LedgerAccount>,
) -> Result<Vec<VoteWithWeight>> {
    let key = key.into();
    let values = process_votes(key, votes, chain_tip);

    Ok(values
        .into_iter()
        .filter_map(|vote| {
            let stake_weight = get_stake_weight(&ledger, &vote.account).ok()?;
            Some(VoteWithWeight::new(vote, stake_weight))
        })
        .collect::<Vec<VoteWithWeight>>())
}

fn decode_memo(key: &str, encoded: &str) -> Option<String> {
    if let Ok((_ver, bytes)) = encoded.from_base58check() {
        if *bytes.first()? != 1u8 {
            return None;
        };
        let end_idx = *bytes.get(1)? as usize + 2;
        match std::str::from_utf8(&bytes[2..end_idx]) {
            Ok(str) => match str.to_lowercase().contains(&key.to_lowercase()) {
                true => Some(str.to_string()),
                false => None,
            },
            Err(_) => None,
        }
    } else {
        None
    }
}

fn match_memo(key: &str, memo: &str) -> bool {
    memo.to_lowercase() == key.to_lowercase()
        || memo.to_lowercase() == f!("no {}", key.to_lowercase())
}

fn is_newer(a: &Vote, b: &Vote) -> bool {
    a.height > b.height || (a.height == b.height && a.nonce > b.nonce)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{BlockStatus, Vote};

    fn get_votes() -> (Vote, Vote, Vote, Vote) {
        return (
            Vote::new(
                "1",
                "1",
                "E4YjFkHVUXbEAkQcUrAEcS1fqvbncnn9Tuz2Jtb1Uu79zY9UAJRpd",
                100,
                BlockStatus::Pending,
                100,
                1,
            ),
            Vote::new(
                "1",
                "2",
                "E4YjFkHVUXbEAkQcUrAEcS1fqvbncnn9Tuz2Jtb1Uu79zY9UAJRpd",
                110,
                BlockStatus::Pending,
                110,
                1,
            ),
            Vote::new(
                "2",
                "3",
                "E4YjFkHVUXbEAkQcUrAEcS1fqvbncnn9Tuz2Jtb1Uu79zY9UAJRpd",
                110,
                BlockStatus::Pending,
                110,
                1,
            ),
            Vote::new(
                "2",
                "4",
                "E4YdLeukpqzqyBAxujeELx9SZWoUW9MhcUfnGHF9PhQmxTJcpmj7j",
                120,
                BlockStatus::Pending,
                120,
                2,
            ),
        );
    }

    #[test]
    fn test_decode_memo() {
        let key = "cftest-2";
        let (s1, s2, s3, s4) = get_votes();
        let s1_decoded = decode_memo(key, &s1.memo).unwrap();
        let s2_decoded = decode_memo(key, &s2.memo).unwrap();
        let s3_decoded = decode_memo(key, &s3.memo).unwrap();
        let s4_decoded = decode_memo(key, &s4.memo).unwrap();

        assert_eq!(s1_decoded, "no cftest-2");
        assert_eq!(s2_decoded, "no cftest-2");
        assert_eq!(s3_decoded, "no cftest-2");
        assert_eq!(s4_decoded, "cftest-2");
    }

    #[test]
    fn test_process_votes() {
        let (s1, s2, s3, s4) = get_votes();
        let processed = process_votes("cftest-2", vec![s1, s2, s3, s4], 129);

        assert_eq!(processed.len(), 2);

        let a1 = processed.iter().find(|s| s.account == "1").unwrap();
        let a2 = processed.iter().find(|s| s.account == "2").unwrap();

        assert_eq!(a1.account, "1");
        assert_eq!(a1.hash, "2");
        assert_eq!(a1.memo, "no cftest-2");
        assert_eq!(a1.height, 110);
        assert_eq!(a1.status, BlockStatus::Canonical);
        assert_eq!(a1.nonce, 1);

        assert_eq!(a2.account, "2");
        assert_eq!(a2.hash, "4");
        assert_eq!(a2.memo, "cftest-2");
        assert_eq!(a2.height, 120);
        assert_eq!(a2.status, BlockStatus::Pending);
        assert_eq!(a2.nonce, 2);
    }
}
