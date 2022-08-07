use actix_web::{
    get,
    http::header::ContentType,
    web::{self, Data, ServiceConfig},
    HttpResponse, Responder,
};
use serde::{Deserialize, Serialize};
use std::sync::Arc;

use crate::{
    db,
    models::{BlockStatus, DBResponse, ResponseEntity, Status},
};

pub fn validate_signal(memo: &str, key: &str) -> bool {
    if memo.to_lowercase() == key.to_lowercase()
        || memo.to_lowercase() == format!("no {}", key.to_lowercase())
    {
        return true;
    };
    false
}

pub fn check_sort(mut v: Vec<DBResponse>, sorted: Option<bool>) -> Vec<DBResponse> {
    if let Some(b) = sorted {
        if b {
            v.sort_by(|a, b| b.height.cmp(&a.height));
            return v;
        } else {
            return v;
        }
    }
    v
} 

pub fn parse_responses(
    query_responses: Vec<DBResponse>,
    key: &str,
    latest_block: i64,
    request_type: Option<QueryRequestFilter>,
    sorted: Option<bool>,
) -> ResponseEntity {
    let mut hash: std::collections::HashMap<String, Vec<DBResponse>> =
        std::collections::HashMap::new();
    let mut settled: std::collections::HashMap<String, DBResponse> =
        std::collections::HashMap::new();
    let mut unsettled: Vec<DBResponse> = Vec::with_capacity(query_responses.len());
    let mut invalid: Vec<DBResponse> = Vec::with_capacity(query_responses.len());

    for res in query_responses.iter() {
        if let Some(memo_str) = crate::decode_memo(&res.memo, key) {
            if validate_signal(&memo_str, key) {
                match hash.get_mut(&res.account) {
                    Some(x) => x.push(DBResponse {
                        account: res.account.clone(),
                        height: res.height,
                        memo: memo_str,
                        status: res.status,
                        timestamp: res.timestamp,
                        signal_status: res.signal_status,
                    }),
                    None => {
                        hash.entry(res.account.clone()).or_insert_with_key(|_| {
                            vec![DBResponse {
                                account: res.account.clone(),
                                height: res.height,
                                memo: memo_str,
                                status: res.status,
                                timestamp: res.timestamp,
                                signal_status: res.signal_status,
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

    let s = settled
        .into_iter()
        .map(|(_, v)| v)
        .collect::<Vec<DBResponse>>();

    match request_type {
        Some(filter) => match filter {
            QueryRequestFilter::All => {
                let mut v: Vec<DBResponse> = vec![];
                v.extend(s);
                v.extend(unsettled);
                v.extend(invalid);

                ResponseEntity { signals: check_sort(v, sorted) }
            }
            QueryRequestFilter::Settled => {
                ResponseEntity { signals: check_sort(s, sorted) }
            }
            QueryRequestFilter::Unsettled => {
                ResponseEntity { signals: check_sort(unsettled, sorted) }
            }
            QueryRequestFilter::Invalid => {
                ResponseEntity {signals: check_sort(invalid, sorted)}
            }
        },
        None => {
            let mut vec: Vec<DBResponse> = vec![];
            vec.extend(s);
            vec.extend(unsettled);
            vec.extend(invalid);

            ResponseEntity { signals: vec }
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
    let response = db::queries::get_memo_data(&pg_client).await.expect("Error: Could not get memo data.");
    let result = parse_responses(
        response,
        &key,
        latest_block_height,
        params.filter,
        params.sorted,
    );

    HttpResponse::Ok()
        .content_type(ContentType::json())
        .body(serde_json::to_string(&result).expect("Error: Could not parse HttpResponse."))
}
