use axum::{
    extract::{Path, Query as AxumQuery},
    http::StatusCode,
    response::IntoResponse,
    Extension,
};
use base58check::FromBase58Check;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::{
    ledger::LedgerDelegations,
    models::{BlockStatus, DBResponse, ResponseEntity, Signal, SignalStats, SignalStatus},
    queries,
};

fn decode_memo(memo: &str, keyword: &str) -> Option<String> {
    if let Ok((_ver, bytes)) = memo.from_base58check() {
        if *bytes.first()? != 1u8 {
            return None;
        };
        let end_idx = *bytes.get(1)? as usize + 2;
        match std::str::from_utf8(&bytes[2..end_idx]) {
            Ok(str) => match str.to_lowercase().contains(keyword) {
                true => Some(str.to_string()),
                false => None,
            },
            Err(_) => None,
        }
    } else {
        None
    }
}

fn validate_signal(memo: &str, key: &str) -> bool {
    if memo.to_lowercase() == key.to_lowercase()
        || memo.to_lowercase() == format!("no {}", key.to_lowercase())
    {
        return true;
    };
    false
}

/// It's so bad.
pub async fn parse_responses(
    key: String,
    latest_block: i64,
    signals: Vec<DBResponse>,
    ctx: Extension<crate::ApiContext>,
    network: QueryRequestFilter,
    req_filter: Option<QueryRequestFilter>,
) -> ResponseEntity {
    ctx.ledger
        .call(move |conn| {
            let mut hash: HashMap<String, Vec<Signal>> = HashMap::new();
            let mut settled: HashMap<String, Signal> = HashMap::new();
            let mut unsettled: Vec<Signal> = Vec::with_capacity(signals.len());
            let mut invalid: Vec<Signal> = Vec::with_capacity(signals.len());
            let mut yes: f32 = 0.00;
            let mut no: f32 = 0.00;

            let mut stmt = conn
                .prepare(
                    "
                SELECT CAST(SUM(CAST(balance AS DECIMAL)) AS TEXT), COUNT(pk) as delegators
                FROM Ledger
                WHERE delegate = (?)
                GROUP BY delegate
            ",
                )
                .expect("Error preparing statement.");

            for res in signals.iter() {
                if let Some(memo_str) = decode_memo(&res.memo, &key) {
                    if validate_signal(&memo_str, &key) {
                        match hash.get_mut(&res.account) {
                            Some(x) => {
                                let rows_iter = stmt
                                    .query_map([res.account.clone()], |row| {
                                        Ok(LedgerDelegations {
                                            delegated_balance: row.get(0).unwrap_or_default(),
                                            total_delegators: row.get(1).unwrap_or_default(),
                                        })
                                    })
                                    .expect("Error: Error unwrapping rows.");

                                match network {
                                    QueryRequestFilter::Mainnet => {
                                        let mut stake: LedgerDelegations =
                                            LedgerDelegations::default();

                                        for res in rows_iter {
                                            match res {
                                                Ok(x) => stake = x,
                                                Err(err) => println!("{}", err),
                                            }
                                        }

                                        if !stake.is_default() {
                                            if memo_str.to_lowercase() == key.to_lowercase() {
                                                yes += stake
                                                    .delegated_balance
                                                    .parse::<f32>()
                                                    .unwrap_or(0.00)
                                            }

                                            if memo_str.to_lowercase()
                                                == format!("no {}", key.to_lowercase())
                                            {
                                                no += stake
                                                    .delegated_balance
                                                    .parse::<f32>()
                                                    .unwrap_or(0.00)
                                            }

                                            x.push(Signal {
                                                account: res.account.clone(),
                                                height: res.height,
                                                memo: memo_str,
                                                status: res.status,
                                                timestamp: res.timestamp,
                                                signal_status: None,
                                                delegations: Some(stake),
                                            })
                                        }
                                    }
                                    _ => x.push(Signal {
                                        account: res.account.clone(),
                                        height: res.height,
                                        memo: memo_str,
                                        status: res.status,
                                        timestamp: res.timestamp,
                                        signal_status: None,
                                        delegations: None,
                                    }),
                                }
                            }
                            None => {
                                let rows_iter = stmt
                                    .query_map([res.account.clone()], |row| {
                                        Ok(LedgerDelegations {
                                            delegated_balance: row.get(0).unwrap_or_default(),
                                            total_delegators: row.get(1).unwrap_or_default(),
                                        })
                                    })
                                    .expect("Error: Error unwrapping rows.");

                                match network {
                                    QueryRequestFilter::Mainnet => {
                                        let mut stake: LedgerDelegations =
                                            LedgerDelegations::default();

                                        for res in rows_iter {
                                            match res {
                                                Ok(x) => stake = x,
                                                Err(err) => println!("{}", err),
                                            }
                                        }

                                        if !stake.is_default() {
                                            if memo_str.to_lowercase() == key.to_lowercase() {
                                                yes += stake
                                                    .delegated_balance
                                                    .parse::<f32>()
                                                    .unwrap_or(0.00)
                                            }

                                            if memo_str.to_lowercase()
                                                == format!("no {}", key.to_lowercase())
                                            {
                                                no += stake
                                                    .delegated_balance
                                                    .parse::<f32>()
                                                    .unwrap_or(0.00)
                                            }

                                            hash.entry(res.account.clone()).or_insert_with_key(
                                                |_| {
                                                    vec![Signal {
                                                        account: res.account.clone(),
                                                        height: res.height,
                                                        memo: memo_str,
                                                        status: res.status,
                                                        timestamp: res.timestamp,
                                                        signal_status: None,
                                                        delegations: match stake.is_default() {
                                                            true => None,
                                                            false => Some(stake),
                                                        },
                                                    }]
                                                },
                                            );
                                        }
                                    }
                                    _ => {
                                        hash.entry(res.account.clone()).or_insert_with_key(|_| {
                                            vec![Signal {
                                                account: res.account.clone(),
                                                height: res.height,
                                                memo: memo_str,
                                                status: res.status,
                                                timestamp: res.timestamp,
                                                signal_status: None,
                                                delegations: None,
                                            }]
                                        });
                                    }
                                }
                            }
                        }
                    } else {
                        let rows_iter = stmt
                            .query_map([res.account.clone()], |row| {
                                Ok(LedgerDelegations {
                                    delegated_balance: row.get(0).unwrap_or_default(),
                                    total_delegators: row.get(1).unwrap_or_default(),
                                })
                            })
                            .expect("Error: Error unwrapping rows.");

                        match network {
                            QueryRequestFilter::Mainnet => {
                                let mut stake: LedgerDelegations = LedgerDelegations::default();

                                for res in rows_iter {
                                    match res {
                                        Ok(x) => stake = x,
                                        Err(err) => println!("{}", err),
                                    }
                                }

                                if !stake.is_default() {
                                    if memo_str.to_lowercase() == key.to_lowercase() {
                                        yes +=
                                            stake.delegated_balance.parse::<f32>().unwrap_or(0.00)
                                    }

                                    if memo_str.to_lowercase()
                                        == format!("no {}", key.to_lowercase())
                                    {
                                        no += stake.delegated_balance.parse::<f32>().unwrap_or(0.00)
                                    }

                                    invalid.push(Signal {
                                        account: res.account.clone(),
                                        memo: memo_str,
                                        height: res.height,
                                        status: res.status,
                                        timestamp: res.timestamp,
                                        signal_status: Some(SignalStatus::Invalid),
                                        delegations: Some(stake),
                                    })
                                }
                            }
                            _ => invalid.push(Signal {
                                account: res.account.clone(),
                                memo: memo_str,
                                height: res.height,
                                status: res.status,
                                timestamp: res.timestamp,
                                signal_status: Some(SignalStatus::Invalid),
                                delegations: None,
                            }),
                        }
                    }
                }
            }

            for (_, v) in hash.into_iter() {
                for mut i in v.into_iter() {
                    match settled.get_mut(&i.account) {
                        Some(x) => {
                            if i.height > x.height
                                && i.height + crate::constants::SETTLED_DENOMINATOR <= latest_block
                                && matches!(i.status, BlockStatus::Canonical)
                            {
                                i.signal_status = Some(SignalStatus::Settled);
                                *x = i
                            } else {
                                i.signal_status = Some(SignalStatus::Unsettled);
                                unsettled.push(i)
                            }
                        }
                        None => {
                            if i.height + crate::constants::SETTLED_DENOMINATOR <= latest_block
                                && matches!(i.status, BlockStatus::Canonical)
                            {
                                i.signal_status = Some(SignalStatus::Settled);
                                settled.insert(i.account.clone(), i);
                            } else {
                                i.signal_status = Some(SignalStatus::Unsettled);
                                unsettled.push(i)
                            }
                        }
                    }
                }
            }

            let settled_vec = settled.into_iter().map(|(_, v)| v).collect::<Vec<Signal>>();
            let statistics = SignalStats { yes, no };

            match req_filter {
                Some(filter) => match filter {
                    QueryRequestFilter::All => {
                        ResponseEntity::new([settled_vec, unsettled, invalid].concat())
                            .with_stats(statistics)
                            .sort()
                    }
                    QueryRequestFilter::Settled => ResponseEntity::new(settled_vec)
                        .with_stats(statistics)
                        .sort(),
                    QueryRequestFilter::Unsettled => {
                        ResponseEntity::new(unsettled).with_stats(statistics).sort()
                    }
                    QueryRequestFilter::Invalid => {
                        ResponseEntity::new(invalid).with_stats(statistics).sort()
                    }
                    _ => ResponseEntity::new([settled_vec, unsettled, invalid].concat())
                        .with_stats(statistics)
                        .sort(),
                },
                None => ResponseEntity::new([settled_vec, unsettled, invalid].concat())
                    .with_stats(statistics)
                    .sort(),
            }
        })
        .await
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum QueryRequestFilter {
    All,
    Settled,
    Unsettled,
    Invalid,
    Mainnet,
    Devnet,
}

pub async fn handler(
    Path(key): Path<String>,
    AxumQuery(mut params): AxumQuery<HashMap<String, QueryRequestFilter>>,
    ctx: Extension<crate::ApiContext>,
) -> impl IntoResponse {
    let network_opt = params.remove("network");
    let filter = params.remove("filter");

    if let Some(network) = network_opt {
        let signals = match network {
            QueryRequestFilter::Mainnet => queries::get_signals(&ctx.mainnet_db)
                .await
                .expect("Error: Could not get mainnet signals."),
            _ => queries::get_signals(&ctx.devnet_db)
                .await
                .expect("Error: Could not get devnet signals."),
        };

        let latest_block_height = queries::get_latest_blockheight(&ctx, &network)
            .await
            .expect("Error: Could not get latest block.");
        let result = parse_responses(key, latest_block_height, signals, ctx, network, filter).await;

        return (StatusCode::ACCEPTED, axum::Json(result)).into_response();
    }

    return (
        StatusCode::BAD_REQUEST,
        axum::Json("Error: Network param not provided."),
    )
        .into_response();
}
