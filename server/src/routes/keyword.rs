use axum::{
    extract::{Path, Query as AxumQuery},
    http::StatusCode,
    response::IntoResponse,
    Extension,
};
use base58check::{FromBase58Check, ToBase58Check};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::{
    ledger::LedgerDelegations,
    models::{BlockStatus, DBResponse, ResponseEntity, Signal, SignalStats, SignalStatus},
    queries,
};

pub fn mina_encode(memo: &str) -> String {
    let bytes = memo.as_bytes();
    let mut encoded = Vec::new();
    encoded.push(1 as u8);
    encoded.push(memo.len() as u8);
    for byte in bytes.iter() {
        encoded.push(*byte);
    }

    encoded.as_slice().to_base58check(0)
}

pub fn decode_memo(memo: &str) -> Option<String> {
    if let Ok((_ver, bytes)) = memo.from_base58check() {
        if *bytes.first()? != 1u8 {
            return None;
        };
        let end_idx = *bytes.get(1)? as usize + 2;
        match std::str::from_utf8(&bytes[2..end_idx]) {
            Ok(str) => Some(str.to_string()),
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

fn add_signal_to_existing_account(
    stmt: &mut rusqlite::Statement,
    memo_str: &String,
    key: &String,
    res: &DBResponse,
    yes: &mut f32,
    no: &mut f32,
    signals: &mut Vec<Signal>,
) {
    let rows_iter = stmt
        .query_map([res.account.clone()], |row| {
            Ok(LedgerDelegations {
                delegated_balance: row.get(0).unwrap_or_default(),
                total_delegators: row.get(1).unwrap_or_default(),
            })
        })
        .expect("Error: Error unwrapping rows.");

    let mut stake: LedgerDelegations = LedgerDelegations::default();

    for res in rows_iter {
        match res {
            Ok(x) => stake = x,
            Err(err) => println!("{}", err),
        }
    }

    if !stake.is_default() {
        if memo_str.to_lowercase() == key.to_lowercase() {
            *yes += stake.delegated_balance.parse::<f32>().unwrap_or(0.00)
        }

        if memo_str.to_lowercase() == format!("no {}", key.to_lowercase()) {
            *no += stake.delegated_balance.parse::<f32>().unwrap_or(0.00)
        }

        signals.push(Signal {
            account: res.account.clone(),
            height: res.height,
            memo: memo_str.clone(),
            status: res.status,
            timestamp: res.timestamp,
            signal_status: None,
            delegations: Some(stake),
        })
    }
}

fn add_signal_to_new_account(
    stmt: &mut rusqlite::Statement,
    memo_str: &String,
    key: &String,
    res: &DBResponse,
    yes: &mut f32,
    no: &mut f32,
    hash: &mut HashMap<String, Vec<Signal>>,
) {
    let rows_iter = stmt
        .query_map([res.account.clone()], |row| {
            Ok(LedgerDelegations {
                delegated_balance: row.get(0).unwrap_or_default(),
                total_delegators: row.get(1).unwrap_or_default(),
            })
        })
        .expect("Error: Error unwrapping rows.");

    let mut stake: LedgerDelegations = LedgerDelegations::default();

    for res in rows_iter {
        match res {
            Ok(x) => stake = x,
            Err(err) => println!("{}", err),
        }
    }

    if !stake.is_default() {
        if memo_str.to_lowercase() == key.to_lowercase() {
            *yes += stake.delegated_balance.parse::<f32>().unwrap_or(0.00)
        }

        if memo_str.to_lowercase() == format!("no {}", key.to_lowercase()) {
            *no += stake.delegated_balance.parse::<f32>().unwrap_or(0.00)
        }

        hash.entry(res.account.clone()).or_insert_with_key(|_| {
            vec![Signal {
                account: res.account.clone(),
                height: res.height,
                memo: memo_str.clone(),
                status: res.status,
                timestamp: res.timestamp,
                signal_status: None,
                delegations: match stake.is_default() {
                    true => None,
                    false => Some(stake),
                },
            }]
        });
    }
}

fn process_invalid_signal(
    stmt: &mut rusqlite::Statement,
    res: &DBResponse,
    memo_str: &String,
    key: &String,
    yes: &mut f32,
    no: &mut f32,
    invalid: &mut Vec<Signal>,
) {
    let rows_iter = stmt
        .query_map([res.account.clone()], |row| {
            Ok(LedgerDelegations {
                delegated_balance: row.get(0).unwrap_or_default(),
                total_delegators: row.get(1).unwrap_or_default(),
            })
        })
        .expect("Error: Error unwrapping rows.");

    let mut stake: LedgerDelegations = LedgerDelegations::default();

    for res in rows_iter {
        match res {
            Ok(x) => stake = x,
            Err(err) => println!("{}", err),
        }
    }

    if !stake.is_default() {
        if memo_str.to_lowercase() == key.to_lowercase() {
            *yes += stake.delegated_balance.parse::<f32>().unwrap_or(0.00)
        }

        if memo_str.to_lowercase() == format!("no {}", key.to_lowercase()) {
            *no += stake.delegated_balance.parse::<f32>().unwrap_or(0.00)
        }

        invalid.push(Signal {
            account: res.account.clone(),
            memo: memo_str.clone(),
            height: res.height,
            status: res.status,
            timestamp: res.timestamp,
            signal_status: Some(SignalStatus::Invalid),
            delegations: Some(stake),
        })
    }
}

fn process_signal(
    memo_str: &String,
    key: &String,
    hash: &mut HashMap<String, Vec<Signal>>,
    res: &DBResponse,
    stmt: &mut rusqlite::Statement,
    yes: &mut f32,
    no: &mut f32,
    invalid: &mut Vec<Signal>,
) {
    if validate_signal(&memo_str, &key) {
        match hash.get_mut(&res.account) {
            Some(signals) => {
                add_signal_to_existing_account(stmt, memo_str, key, res, yes, no, signals)
            }
            None => add_signal_to_new_account(stmt, memo_str, key, res, yes, no, hash),
        }
    } else {
        process_invalid_signal(stmt, res, memo_str, key, yes, no, invalid);
    }
}

#[derive(Debug, Clone, PartialEq)]
struct BlockProcessingResult {
    hash: HashMap<String, Vec<Signal>>,
    invalid: Vec<Signal>,
    yes: f32,
    no: f32,
}

fn iter_signals(
    signals: &Vec<DBResponse>,
    key: String,
    stmt: &mut rusqlite::Statement,
) -> BlockProcessingResult {
    let mut hash: HashMap<String, Vec<Signal>> = HashMap::new();
    let mut invalid: Vec<Signal> = Vec::with_capacity(signals.len());
    let (mut yes, mut no) = (0.0, 0.0);
    for res in signals.iter() {
        if let Some(memo_str) = decode_memo(&res.memo) {
            process_signal(
                &memo_str,
                &key,
                &mut hash,
                res,
                stmt,
                &mut yes,
                &mut no,
                &mut invalid,
            );
        }
    }

    BlockProcessingResult {
        hash,
        invalid,
        yes,
        no,
    }
}

fn classify_settled_unsettled(
    hash: HashMap<String, Vec<Signal>>,
    signals: &Vec<DBResponse>,
    latest_block: i64,
) -> (HashMap<String, Signal>, Vec<Signal>) {
    let mut settled: HashMap<String, Signal> = HashMap::new();
    let mut unsettled: Vec<Signal> = Vec::with_capacity(signals.len());
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

    (settled, unsettled)
}

pub fn construct_responses(
    conn: &mut rusqlite::Connection,
    key: String,
    latest_block: i64,
    signals: Vec<DBResponse>,
) -> ResponseEntity {
    let mut stmt = conn
        .prepare(
            "
            SELECT 
                CAST(
                    SUM(
                        CAST(
                            balance AS DECIMAL
                        )
                    ) AS TEXT
                ), 
                COUNT(pk) as delegators
            FROM Ledger
            WHERE delegate = (?)
            GROUP BY delegate
        ",
        )
        .expect("Error preparing statement.");

    let block_result = iter_signals(&signals, key, &mut stmt);

    let (settled, unsettled) =
        classify_settled_unsettled(block_result.hash, &signals, latest_block);

    let settled_vec = settled.into_iter().map(|(_, v)| v).collect::<Vec<Signal>>();
    let statistics = SignalStats {
        yes: block_result.yes,
        no: block_result.no,
    };

    ResponseEntity::new(settled_vec, unsettled, block_result.invalid)
        .with_stats(statistics)
        .sort()
}

pub async fn parse_responses(
    key: String,
    latest_block: i64,
    signals: Vec<DBResponse>,
    ctx: Extension<crate::ApiContext>,
    network: QueryRequestFilter,
) -> ResponseEntity {
    match network {
        QueryRequestFilter::Mainnet => {
            ctx.mainnet_ledger
                .call(move |conn| construct_responses(conn, key, latest_block, signals))
                .await
        }
        QueryRequestFilter::Devnet => {
            ctx.devnet_ledger
                .call(move |conn| construct_responses(conn, key, latest_block, signals))
                .await
        }
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
        let result = parse_responses(key, latest_block_height, signals, ctx, network).await;

        return (StatusCode::ACCEPTED, axum::Json(result)).into_response();
    }

    (
        StatusCode::BAD_REQUEST,
        axum::Json("Error: Network param not provided."),
    )
        .into_response()
}
