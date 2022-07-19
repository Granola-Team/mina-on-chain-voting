use std::sync::Arc;
use actix_web::{get, web, Responder, HttpResponse, http::header::ContentType};
use crate::{db, remove_duplicates};

#[get("/votes")]
pub async fn votes(
    pg_client: web::Data<Arc<tokio_postgres::Client>>
) -> impl Responder {
    let response = db::queries::get_canonical_votes_query(&pg_client).await.unwrap();
    let result = remove_duplicates(&response);
    HttpResponse::Ok().content_type(ContentType::json()).body(serde_json::to_string(&result).unwrap())
}
