use std::sync::Arc;
use serde::{Serialize, Deserialize};
use actix_web::{get, web::{ServiceConfig, Data}, Responder, HttpResponse, http::header::ContentType};

use crate::db::{self, queries::{QueryResponse, BlockStatus}};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VoterEntity {
    pub account: String,
    pub votes: Vec<Vote>
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Vote {
    pub memo: String,
    pub height: i64,
    pub status: BlockStatus
}

pub type VoterMap = std::collections::HashMap<String, Vec<Vote>>;

pub fn parse_responses(query_responses: &[QueryResponse]) -> VoterMap {
    let mut hash: VoterMap = std::collections::HashMap::new();

    for res in query_responses.iter() {
        if let Some(memo_str) = crate::decode_memo(&res.memo) {
            match hash.get_mut(&res.account) {
                Some(x) => {
                    x.push(Vote { memo: memo_str, height: res.height, status: res.status })
                },
                None => {
                    hash.entry(res.account.clone()).or_insert_with_key(|_| vec![Vote { memo: memo_str, height: res.height, status: res.status }]);
                }
            }
        } 
    }
    
    hash
}

pub fn config(cfg: &mut ServiceConfig) {
    cfg.service(votes);
}

#[get("/votes")]
pub async fn votes(
    pg_client: Data<Arc<tokio_postgres::Client>>
) -> impl Responder {
    let response = db::queries::get_canonical_and_pending_votes_query(&pg_client).await.unwrap();
    let result = parse_responses(&response);

    HttpResponse::Ok().content_type(ContentType::json()).body(serde_json::to_string(&result).unwrap())
}

