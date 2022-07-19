use std::sync::Arc;
use actix_web::{get, web, Responder, HttpResponse, http::header::ContentType};
use crate::{gen_output, parse_query_response, db, decode_memo};


#[get("/votes")]
pub async fn votes(
    pg_client: web::Data<Arc<tokio_postgres::Client>>
) -> impl Responder {
    let response = db::queries::get_canonical_votes_query(&pg_client).await.unwrap();

    for i in response {
        if let Some(x) = decode_memo(&i.memo) {
            println!("{:?}", x);
        }
    }

    // let votes_map = parse_query_response(&response);

    // println!("{:?}", votes_map);

    // let output = serde_json::to_string(&gen_output(votes_map)).unwrap();

    HttpResponse::Ok().content_type(ContentType::json()).body("HELLO".to_string())
}
