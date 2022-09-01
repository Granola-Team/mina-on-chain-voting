use axum::{extract::{Path, Query as AxumQuery}, Extension, response::IntoResponse, http::StatusCode};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use base58check::FromBase58Check;

use crate::{models::{BlockStatus, DBResponse, ResponseEntity,Signal, SignalStats, SignalStatus}, queries, ledger::LedgerDelegations};

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

pub async fn parse_responses(
    query_responses: Vec<DBResponse>,
    key: String,
    latest_block: i64,
    req_filter: Option<QueryRequestFilter>,
    ctx: Extension<crate::ApiContext>
) -> ResponseEntity {
    let res = ctx.ledger.call(move|conn| {
        let mut hash: HashMap<String, Vec<Signal>> = HashMap::new();
        let mut settled: HashMap<String, Signal> = HashMap::new();
        let mut unsettled: Vec<Signal> = Vec::with_capacity(query_responses.len());
        let mut invalid: Vec<Signal> = Vec::with_capacity(query_responses.len());
        let mut yes: f32 = 0.00;
        let mut no: f32 = 0.00;

        let mut stmt = conn.prepare(
            "
                SELECT CAST(SUM(CAST(balance AS DECIMAL)) AS TEXT), COUNT(pk) as delegators
                FROM Ledger
                WHERE delegate = (?)
                GROUP BY delegate
            "
            ).expect("Error preparing statement.");

        for res in query_responses.iter() {
            if let Some(memo_str) = decode_memo(&res.memo, &key) {
                if validate_signal(&memo_str, &key) {
                    match hash.get_mut(&res.account) {
                        Some(x) => {
                            let rows_iter = stmt.query_map([res.account.clone()], |row| {
                                Ok(LedgerDelegations {
                                        delegated_balance: row.get(0).unwrap_or_default(),
                                        total_delegators: row.get(1).unwrap_or_default(),
                                    })
                            }).expect("Error: Error unwrapping rows.");
            
                            let mut stake: LedgerDelegations = LedgerDelegations::default();
            
                            for res in rows_iter {
                                match res {
                                    Ok(x) => { stake = x },
                                    Err(err) => println!("{}", err)
                                }
                            }

                            match stake.is_default() {
                                true => (),
                                false => {
                                    if memo_str.to_lowercase() == key.to_lowercase() { 
                                            yes+=stake.delegated_balance.parse::<f32>().unwrap_or(0.00)
                                    }

                                    if memo_str.to_lowercase() == format!("no {}", key.to_lowercase()) { 
                                            no+=stake.delegated_balance.parse::<f32>().unwrap_or(0.00)
                                    }
                                    
                                    x.push(Signal {
                                        account: res.account.clone(),
                                        height: res.height,
                                        memo: memo_str,
                                        status: res.status,
                                        timestamp: res.timestamp,
                                        signal_status: None,
                                        delegations: Some(stake)
                                    })
                                }
                            }
                        },
                        None => {
                            let rows_iter = stmt.query_map([res.account.clone()], |row| {
                                Ok(LedgerDelegations {
                                        delegated_balance: row.get(0).unwrap_or_default(),
                                        total_delegators: row.get(1).unwrap_or_default(),
                                    })
                            }).expect("Error: Error unwrapping rows.");
            
                            let mut stake: LedgerDelegations = LedgerDelegations::default();
            
                            for res in rows_iter {
                                match res {
                                    Ok(x) => { stake = x },
                                    Err(err) => println!("{}", err)
                                }
                            }

                            match stake.is_default() {
                                true => (),
                                false => {
                                    if memo_str.to_lowercase() == key.to_lowercase() { 
                                        yes+=stake.delegated_balance.parse::<f32>().unwrap_or(0.00)
                                    }

                                    if memo_str.to_lowercase() == format!("no {}", key.to_lowercase()) { 
                                        no+=stake.delegated_balance.parse::<f32>().unwrap_or(0.00)
                                    }

                                    hash.entry(res.account.clone()).or_insert_with_key(|_| {
                                        vec![Signal {
                                            account: res.account.clone(),
                                            height: res.height,
                                            memo: memo_str,
                                            status: res.status,
                                            timestamp: res.timestamp,
                                            signal_status: None,
                                            delegations: match stake.is_default() {
                                                true => { None },
                                                false => { Some(stake) },
                                            },
                                        }]
                                    });
                                }
                            }
                        }
                    }
                } else {
                    let rows_iter = stmt.query_map([res.account.clone()], |row| {
                        Ok(LedgerDelegations {
                                delegated_balance: row.get(0).unwrap_or_default(),
                                total_delegators: row.get(1).unwrap_or_default(),
                            })
                    }).expect("Error: Error unwrapping rows.");
    
                    let mut stake: LedgerDelegations = LedgerDelegations::default();
    
                    for res in rows_iter {
                        match res {
                            Ok(x) => { stake = x },
                            Err(err) => println!("{}", err)
                        }
                    }

                    match stake.is_default() {
                        true => (),
                        false => {
                            if memo_str.to_lowercase() == key.to_lowercase() { 
                                yes+=stake.delegated_balance.parse::<f32>().unwrap_or(0.00)
                            }

                            if memo_str.to_lowercase() == format!("no {}", key.to_lowercase()) { 
                                no+=stake.delegated_balance.parse::<f32>().unwrap_or(0.00)
                            }

                            invalid.push(Signal {
                                account: res.account.clone(),
                                memo: memo_str,
                                height: res.height,
                                status: res.status,
                                timestamp: res.timestamp,
                                signal_status: Some(SignalStatus::Invalid),
                                delegations: Some(stake)
                            })
                        }
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
    
      let settled_vec = settled
            .into_iter()
            .map(|(_, v)| v)
            .collect::<Vec<Signal>>();
    
        let statistics = SignalStats { yes, no };
    
        match req_filter {
            Some(filter) => match filter {
                QueryRequestFilter::All => {
                    ResponseEntity::new([settled_vec, unsettled, invalid].concat()).with_stats(statistics).sort()
                }
                QueryRequestFilter::Settled => {
                    ResponseEntity::new(settled_vec).with_stats(statistics).sort()
                }
                QueryRequestFilter::Unsettled => {
                    ResponseEntity::new(unsettled).with_stats(statistics).sort()
                }
                QueryRequestFilter::Invalid => {
                    ResponseEntity::new(invalid).with_stats(statistics).sort()
                }
            },
            None => {
                ResponseEntity::new([settled_vec, unsettled, invalid].concat()).with_stats(statistics).sort()
            }
        }
        
    }).await;

    res
}


#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum QueryRequestFilter {
    All,
    Settled,
    Unsettled,
    Invalid,
}

pub async fn handler(
    Path(key): Path<String>,
    AxumQuery(mut params): AxumQuery<HashMap<String, QueryRequestFilter>>,
    ctx: Extension<crate::ApiContext>,
) -> impl IntoResponse {
    let filter = params.remove("filter");
    let latest_block_height = queries::get_latest_blockheight(&ctx)
        .await
        .expect("Error: Could not get latest block.");
    let response = queries::get_signals(&ctx).await.expect("Error: Could not get memo data.");
    let result = parse_responses(
        response,
        key,
        latest_block_height,
        filter,
        ctx
    ).await;

    (StatusCode::ACCEPTED, axum::Json(result))
}
