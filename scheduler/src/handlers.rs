use std::sync::Arc;
use actix_web::{get, web};
use crate::database;

#[get("/votes")]
pub async fn votes(
    pg_client: web::Data<Arc<tokio_postgres::Client>>
) -> String {
    serde_json::to_string(
        &database::gen_output(
            database::parse_query_response(
                &database::query_database(&pg_client)
                    .await
                    .unwrap()
                )
            )
        )
        .unwrap()
}