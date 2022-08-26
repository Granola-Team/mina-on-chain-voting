use actix_web::{
    get,
    http::header::ContentType,
    web::{self, Data, ServiceConfig},
    HttpResponse, Responder,
};
use serde::{Deserialize, Serialize};
use std::{sync::Arc, collections::HashMap};

use crate::{
    db,
    models::{BlockStatus, DBResponse, ResponseEntity, Status, SignalStats}, ledger::{Ledger, Query}
};

pub fn validate_signal(memo: &str, key: &str) -> bool {
    if memo.to_lowercase() == key.to_lowercase()
        || memo.to_lowercase() == format!("no {}", key.to_lowercase())
    {
        return true;
    };
    false
}

pub fn parse_responses(
    query_responses: Vec<DBResponse>,
    key: &str,
    latest_block: i64,
    req_filter: Option<QueryRequestFilter>,
    sorted: Option<bool>,
    stats: Option<bool>,
) -> ResponseEntity {
    let mut hash: HashMap<String, Vec<DBResponse>> = HashMap::new();
    let mut settled: HashMap<String, DBResponse> = HashMap::new();
    let mut unsettled: Vec<DBResponse> = Vec::with_capacity(query_responses.len());
    let mut invalid: Vec<DBResponse> = Vec::with_capacity(query_responses.len());

    for res in query_responses.iter() {
        if let Some(memo_str) = crate::decode_memo(&res.memo, key) {
            if validate_signal(&memo_str, key) {
                match hash.get_mut(&res.account) {
                    Some(x) => {
                        x.push(DBResponse {
                            account: res.account.clone(),
                            height: res.height,
                            memo: memo_str,
                            status: res.status,
                            timestamp: res.timestamp,
                            signal_status: res.signal_status,
                            delegations: Ledger::connection().get_stake(&res.account)
                        })
                    },
                    None => {
                        hash.entry(res.account.clone()).or_insert_with_key(|_| {
                            vec![DBResponse {
                                account: res.account.clone(),
                                height: res.height,
                                memo: memo_str,
                                status: res.status,
                                timestamp: res.timestamp,
                                signal_status: res.signal_status,
                                delegations: Ledger::connection().get_stake(&res.account)
                            }]
                        });
                    }
                }
            } else {
                invalid.push(DBResponse {
                    account: res.account.clone(),
                    memo: memo_str,
                    height: res.height,
                    status: res.status,
                    timestamp: res.timestamp,
                    signal_status: Some(Status::Invalid),
                    delegations: Ledger::connection().get_stake(&res.account)
                })
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
                        i.signal_status = Some(Status::Settled);
                        *x = i
                    } else {
                        i.signal_status = Some(Status::Unsettled);
                        unsettled.push(i)
                    }
                }
                None => {
                    if i.height + crate::constants::SETTLED_DENOMINATOR <= latest_block
                        && matches!(i.status, BlockStatus::Canonical)
                    {
                        i.signal_status = Some(Status::Settled);
                        settled.insert(i.account.clone(), i);
                    } else {
                        i.signal_status = Some(Status::Unsettled);
                        unsettled.push(i)
                    }
                }
            }
        }
    }

  let settled_vec = settled
        .into_iter()
        .map(|(_, v)| v)
        .collect::<Vec<DBResponse>>();

  let statistics = match stats {
        Some(s) => match s {
            true => {
                let mut yes: f32 = 0.0;
                let mut no: f32 = 0.0;

                for x in &settled_vec {
                    if x.memo.to_lowercase() == key.to_lowercase() { 
                        if let Some(a) = &x.delegations {
                            yes+=a.delegated_balance.parse::<f32>().unwrap_or(0.00)
                        }
                    }
                    if x.memo.to_lowercase() == format!("no {}", key.to_lowercase()) { 
                        if let Some(a) = &x.delegations {
                            no+=a.delegated_balance.parse::<f32>().unwrap_or(0.00)
                        }
                     }
                }

                for i in &unsettled {
                        if i.memo.to_lowercase() == key.to_lowercase() { 
                            if let Some(a) = &i.delegations {
                                yes+=a.delegated_balance.parse::<f32>().unwrap_or(0.00)
                            }
                          }
                        if i.memo.to_lowercase() == format!("no {}", key.to_lowercase()) {  
                            if let Some(a) = &i.delegations {
                                no+=a.delegated_balance.parse::<f32>().unwrap_or(0.00)
                            }
                         }
                }

                Some(SignalStats { yes, no })
             },
            false  => None
        }
        None => None,
  };    


    match req_filter {
        Some(filter) => match filter {
            QueryRequestFilter::All => {
                ResponseEntity::new([settled_vec, unsettled, invalid].concat()).sorted(sorted).with_stats(stats, statistics)
            }
            QueryRequestFilter::Settled => {
                ResponseEntity::new(settled_vec).sorted(sorted).with_stats(stats, statistics)
            }
            QueryRequestFilter::Unsettled => {
                ResponseEntity::new(unsettled).sorted(sorted).with_stats(stats, statistics)
            }
            QueryRequestFilter::Invalid => {
                ResponseEntity::new(invalid).sorted(sorted).with_stats(stats, statistics)
            }
        },
        None => {
            ResponseEntity::new([settled_vec, unsettled, invalid].concat()).with_stats(stats, statistics)
        }
    }
}

pub fn config(cfg: &mut ServiceConfig) {
    cfg.service(keyword);
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum QueryRequestFilter {
    All,
    Settled,
    Unsettled,
    Invalid,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct KeywordRequest {
    filter: Option<QueryRequestFilter>,
    sorted: Option<bool>,
    stats: Option<bool>,
}

#[get("/{keyword}")]
pub async fn keyword(
    pg_client: Data<Arc<tokio_postgres::Client>>,
    path: web::Path<String>,
    params: web::Query<KeywordRequest>,
) -> impl Responder {
    let key = path.into_inner();
    let latest_block_height = db::queries::get_latest_blockheight(&pg_client)
        .await
        .expect("Error: Could not get latest block.");
    let response = db::queries::get_signals(&pg_client).await.expect("Error: Could not get memo data.");
    let result = parse_responses(
        response,
        &key,
        latest_block_height,
        params.filter,
        params.sorted,
        params.stats,
    );

    HttpResponse::Ok()
        .content_type(ContentType::json())
        .body(serde_json::to_string(&result).expect("Error: Could not parse HttpResponse."))
}
