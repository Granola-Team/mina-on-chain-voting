use std::sync::Arc;
use actix_web::{get, web, Responder, HttpResponse, http::header::ContentType};
use crate::{database, gen_output, parse_query_response};

#[get("/votes")]
pub async fn votes(
    pg_client: web::Data<Arc<tokio_postgres::Client>>
) -> impl Responder {
    let query_responses = database::query_database(&pg_client).await.unwrap();
    let votes_map = parse_query_response(&query_responses);
    let output = serde_json::to_string(&gen_output(votes_map)).unwrap();

    HttpResponse::Ok().content_type(ContentType::json()).body(output)
}