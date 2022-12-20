use axum::{
    extract::{Path, Query as AxumQuery},
    http::StatusCode,
    response::IntoResponse,
    Extension,
};
use base58check::FromBase58Check;
use serde::{Deserialize, Serialize};

use crate::{ledger::get_stake_weight, types::Network};
use crate::{
    ledger::LedgerAccount,
    signal::{Signal, SignalExt, SignalWithWeight},
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

fn is_newer(a: &Signal, b: &Signal) -> bool {
    a.height > b.height || (a.height == b.height && a.nonce > b.nonce)
}

trait SortByTimestamp {
    fn sort_by_timestamp(self) -> Self;
}

impl SortByTimestamp for Vec<Signal> {
    fn sort_by_timestamp(self) -> Self {
        let mut a = self;
        a.sort_by(|a, b| b.timestamp.cmp(&a.timestamp));
        a
    }
}

impl SortByTimestamp for Vec<SignalWithWeight> {
    fn sort_by_timestamp(self) -> Self {
        let mut a = self;
        a.sort_by(|a, b| b.timestamp.cmp(&a.timestamp));
        a
    }
}

fn process_signals(key: impl Into<String>, signals: Vec<Signal>) -> Vec<Signal> {
    let mut map = std::collections::HashMap::new();
    let key = key.into();

    for mut signal in signals {
        if let Some(memo) = decode_memo(&key, &signal.memo) {
            if match_memo(&key, &memo) {
                signal.update_memo(memo);

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
    signals: Vec<Signal>,
    ledger: Vec<LedgerAccount>,
) -> anyhow::Result<Vec<SignalWithWeight>> {
    let key = key.into();
    let values = process_signals(key, signals);

    Ok(values
        .into_iter()
        .filter_map(|signal| {
            let stake_weight = get_stake_weight(&ledger, &signal.account).ok()?;
            Some(SignalWithWeight::new(signal, stake_weight))
        })
        .collect::<Vec<SignalWithWeight>>())
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
        Network::Mainnet => {
            crate::queries::get_signals(
                &ctx.mainnet_db,
                &ctx.signal_cache,
                params.start,
                params.end,
                params.network,
            )
            .await
        }
    };

    let raw = match _raw {
        Ok(raw) => raw,
        Err(_) => {
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                "Error: Failed to get Signals.",
            )
                .into_response()
        }
    };

    let signals = process_signals(key, raw);
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
        Network::Mainnet => {
            crate::queries::get_signals(
                &ctx.mainnet_db,
                &ctx.signal_cache,
                params.start,
                params.end,
                params.network,
            )
            .await
        }
    };

    let raw = match _raw {
        Ok(raw) => raw,
        Err(_) => {
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

    let signals = process_signals_results(key, raw, ledger);

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
