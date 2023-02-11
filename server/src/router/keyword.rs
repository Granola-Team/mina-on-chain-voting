use axum::{
    extract::{Path, Query as AxumQuery},
    http::StatusCode,
    response::IntoResponse,
    Extension,
};
use base58check::FromBase58Check;
use serde::{Deserialize, Serialize};

use crate::prelude::*;
use crate::{ledger::get_stake_weight, queries::get_latest_blockheight, types::Network};
use crate::{
    ledger::LedgerAccount,
    types::{Vote, VoteExt, VoteWithWeight},
};

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
        || memo.to_lowercase() == format!("no {}", key.to_lowercase())
}

fn is_newer(a: &Vote, b: &Vote) -> bool {
    a.height > b.height || (a.height == b.height && a.nonce > b.nonce)
}

trait SortByTimestamp {
    fn sort_by_timestamp(self) -> Self;
}

impl SortByTimestamp for Vec<Vote> {
    fn sort_by_timestamp(self) -> Self {
        let mut a = self;
        a.sort_by(|a, b| b.timestamp.cmp(&a.timestamp));
        a
    }
}

impl SortByTimestamp for Vec<VoteWithWeight> {
    fn sort_by_timestamp(self) -> Self {
        let mut a = self;
        a.sort_by(|a, b| b.timestamp.cmp(&a.timestamp));
        a
    }
}

fn process_signals(key: impl Into<String>, signals: Vec<Vote>, latest_height: i64) -> Vec<Vote> {
    let mut map = std::collections::HashMap::new();
    let key = key.into();

    for mut signal in signals {
        if let Some(memo) = decode_memo(&key, &signal.memo) {
            if match_memo(&key, &memo) {
                signal.update_memo(memo);

                if latest_height - signal.height >= 10 {
                    signal.mark_canonical();
                }

                if let Some(current_signal) = map.get_mut(&signal.account) {
                    if is_newer(&signal, current_signal) {
                        *current_signal = signal;
                    }
                } else {
                    map.insert(signal.account.clone(), signal);
                }
            }
        }
    }
    map.values().cloned().collect()
}

fn process_signals_results(
    key: impl Into<String>,
    signals: Vec<Vote>,
    latest_height: i64,
    ledger: Vec<LedgerAccount>,
) -> Result<Vec<VoteWithWeight>> {
    let key = key.into();
    let values = process_signals(key, signals, latest_height);

    Ok(values
        .into_iter()
        .filter_map(|signal| {
            let stake_weight = get_stake_weight(&ledger, &signal.account).ok()?;
            Some(VoteWithWeight::new(signal, stake_weight))
        })
        .collect::<Vec<VoteWithWeight>>())
}

#[derive(Debug, Serialize, Deserialize)]
pub struct KeywordQueryParams {
    network: Network,
    start: i64,
    end: i64,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ResultsQueryParams {
    network: Network,
    start: i64,
    end: i64,
    hash: String,
}

pub async fn keyword_handler(
    Path(key): Path<String>,
    AxumQuery(params): AxumQuery<KeywordQueryParams>,
    ctx: Extension<crate::APIContext>,
) -> impl IntoResponse {
    let _raw = match params.network {
        Network::Mainnet => (
            crate::queries::get_signals(
                &ctx.mainnet_db,
                &ctx.votes_cache,
                params.start,
                params.end,
                params.network,
            )
            .await,
            get_latest_blockheight(&ctx.mainnet_db).await,
        ),
    };

    let (raw, latest_height) = match _raw {
        (Ok(raw), Ok(hello)) => (raw, hello),
        _ => {
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                "Error: Failed to get Signals.",
            )
                .into_response()
        }
    };

    let signals = process_signals(key, raw, latest_height);
    (
        StatusCode::ACCEPTED,
        axum::Json(signals.sort_by_timestamp()),
    )
        .into_response()
}

pub async fn keyword_results_handler(
    Path(key): Path<String>,
    AxumQuery(params): AxumQuery<ResultsQueryParams>,
    ctx: Extension<crate::APIContext>,
) -> impl IntoResponse {
    let _raw = match params.network {
        Network::Mainnet => (
            crate::queries::get_signals(
                &ctx.mainnet_db,
                &ctx.votes_cache,
                params.start,
                params.end,
                params.network,
            )
            .await,
            get_latest_blockheight(&ctx.mainnet_db).await,
        ),
    };

    let (raw, latest_height) = match _raw {
        (Ok(raw), Ok(hello)) => (raw, hello),
        _ => {
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                "Error: Failed to get Signals.",
            )
                .into_response()
        }
    };
    let _ledger = crate::ledger::get_ledger(params.hash, &ctx.ledger_cache).await;
    let ledger = match _ledger {
        Ok(ledger) => ledger,
        Err(_) => {
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                "Error: Failed to get Ledger.",
            )
                .into_response()
        }
    };

    let signals = process_signals_results(key, raw, latest_height, ledger);

    match signals {
        Ok(signals) => (
            StatusCode::ACCEPTED,
            axum::Json(signals.sort_by_timestamp()),
        )
            .into_response(),
        Err(_) => (
            StatusCode::INTERNAL_SERVER_ERROR,
            "Error: Failed to process Signals.",
        )
            .into_response(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{BlockStatus, Vote};

    fn get_signals() -> (Vote, Vote, Vote, Vote) {
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
        let (s1, s2, s3, s4) = get_signals();
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
    fn test_process_signals() {
        let (s1, s2, s3, s4) = get_signals();
        let processed = process_signals("cftest-2", vec![s1, s2, s3, s4], 129);

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
        assert_eq!(a2.status, BlockStatus::Canonical);
        assert_eq!(a2.nonce, 2);
    }
}
