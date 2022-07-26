use std::sync::Arc;
use actix_web::{get, web::{ServiceConfig, Data, self}, Responder, HttpResponse, http::header::ContentType};
use crate::{db, models::{DBResponse, ResponseEntity, Status}};

pub fn validate_signal(memo: &str, key: &str) -> bool {
    if memo.to_lowercase() == key.to_lowercase() || memo.to_lowercase() == format!("no {}", key.to_lowercase()) { return true };
    false
}

pub fn parse_responses(query_responses: Vec<DBResponse>, key: &str, latest_block: i64) -> Vec<ResponseEntity> {
    let mut hash: std::collections::HashMap<String, Vec<DBResponse>> = std::collections::HashMap::new();
    let mut settled: std::collections::HashMap<String, DBResponse> = std::collections::HashMap::new();
    let mut unsettled: Vec<DBResponse> = Vec::with_capacity(query_responses.len());
    let mut invalid: Vec<DBResponse> = Vec::with_capacity(query_responses.len());

    for res in query_responses.iter() {
        if let Some(memo_str) = crate::decode_memo(&res.memo, key) {
           if validate_signal(&memo_str, key) {
            match hash.get_mut(&res.account) {
                Some(x) => {
                    x.push(DBResponse 
                        { account: res.account.clone(), height: res.height, memo: memo_str, status: res.status, timestamp: res.timestamp })
                },
                None => {
                    hash.entry(res.account.clone()).or_insert_with_key(|_| vec![
                        DBResponse 
                        { account: res.account.clone(), height: res.height, memo: memo_str, status: res.status, timestamp: res.timestamp }
                        ]);
                }
            }
           } else {
            invalid.push(
                DBResponse 
                { account: res.account.clone(), memo: memo_str, height: res.height, status: res.status, timestamp: res.timestamp }
            )
           }
        } 
    }

    for (_, v) in hash.into_iter() {
        for i in v.into_iter() {
            match settled.get_mut(&i.account) {
                Some(x) => { 
                    if i.height > x.height && i.height + crate::constants::SETTLED_DENOMINATOR <= latest_block {
                        *x = i
                    } else {
                        unsettled.push(i)
                    }
                 },
                None => { 
                    if i.height + crate::constants::SETTLED_DENOMINATOR <= latest_block {
                    settled.insert(i.account.clone(), i);
                    } else {
                        unsettled.push(i)
                    }
                 },

            }
        }
    }

    vec![
        ResponseEntity { status:Status::Settled, results: settled.into_iter().map(|(_,v)| { v }).collect::<Vec<DBResponse>>() }, ResponseEntity { status: Status::Unsettled, results: unsettled },
        ResponseEntity { status: Status::Invalid, results: invalid }
        ]
}

pub fn config(cfg: &mut ServiceConfig) {
    cfg.service(keyword);
}

#[get("/{keyword}")]
pub async fn keyword(
    pg_client: Data<Arc<tokio_postgres::Client>>,
    path: web::Path<String>
) -> impl Responder {
    let key = path.into_inner();
    let latest_block_height = db::queries::get_latest_blockheight(&pg_client).await.unwrap();
    let response = db::queries::get_memo_data(&pg_client).await.unwrap();
    let result = parse_responses(response, &key, latest_block_height);

    HttpResponse::Ok().content_type(ContentType::json()).body(serde_json::to_string(&result).unwrap())
}

