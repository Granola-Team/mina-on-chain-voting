use std::sync::Arc;
use serde::{Serialize, Deserialize};
use actix_web::{get, web::{ServiceConfig, Data, self}, Responder, HttpResponse, http::header::ContentType};
use crate::db::{self, queries::{QueryResponse, BlockStatus}};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResultEntity {
    pub account: String,
    pub results: Vec<Result>
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Result {
    pub memo: String,
    pub height: i64,
    pub status: BlockStatus,
    pub timestamp: i64
}

pub fn parse_responses(query_responses: &[QueryResponse], key: &str) -> Vec<ResultEntity> {
    let mut hash: std::collections::HashMap<String, Vec<Result>> = std::collections::HashMap::new();

    for res in query_responses.iter() {
        if let Some(memo_str) = crate::decode_memo(&res.memo, key) {
            match hash.get_mut(&res.account) {
                Some(x) => {
                    x.push(Result { memo: memo_str, height: res.height, status: res.status, timestamp: res.timestamp })
                },
                None => {
                    hash.entry(res.account.clone()).or_insert_with_key(|_| vec![Result { memo: memo_str, height: res.height, status: res.status, timestamp: res.timestamp }]);
                }
            }
        } 
    }
    
    hash.into_iter().map(|(k,r)| {
        ResultEntity {
            account: k,
            results: r
        }
    }).collect::<Vec<ResultEntity>>()


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
    let response = db::queries::send_query(&pg_client).await.unwrap();
    let result = parse_responses(&response, &key);

    HttpResponse::Ok().content_type(ContentType::json()).body(serde_json::to_string(&result).unwrap())
}

