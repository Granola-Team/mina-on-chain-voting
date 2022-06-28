extern crate postgres;
extern crate serde_derive;

use actix_web::{
    body::BoxBody, get, http::header::ContentType, App, Either, HttpResponse, HttpServer, Responder,
};
use serde_derive::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct VotesResponse {
    votes_for: u64, // maybe a little optimistic to use 64 bit ints
    votes_against: u64,
}

impl Responder for VotesResponse {
    type Body = BoxBody;

    fn respond_to(self, _req: &actix_web::HttpRequest) -> HttpResponse<Self::Body> {
        let body = serde_json::to_string(&self).expect("unreachable");

        HttpResponse::Ok()
            .content_type(ContentType::json())
            .body(body)
    }
}

#[get("/votes")]
pub async fn votes() -> Either<VotesResponse, HttpResponse> {
    use postgres::{Client, NoTls};

    //* Connect to the Postgres Database */
    let mut config = Client::configure();
    config.host("localhost");
    config.user("postgres");
    let mut client = match config.connect(NoTls) {
        Ok(client) => client,
        Err(e) => return Either::Right(HttpResponse::InternalServerError().body(e.to_string())),
    };

    //* Query the database for the memo fields */
    let rows = match client.query(
        "
        SELECT user_commands, account_identifiers
        FROM account_identifiers
        FULL JOIN user_commands ON account_identifiers.id = user_commands.id;
        WHERE token_id = /* Mina tokens */ // TODO: what goes here?
        AND type = “payment”
    ",
        &[],
    ) {
        Ok(rows) => rows,
        Err(e) => return Either::Right(HttpResponse::InternalServerError().body(e.to_string())),
    };

    //* Initialize the response struct and parse the memo fields */
    let mut resp = VotesResponse {
        votes_for: 0,
        votes_against: 0,
    };
    rows.iter()
        .map(|row| row.get::<usize, &str>(9 /* the memo field is the 10th row */))
        .for_each(|str| match str {
            // TODO: expand parsing to cover more situations than just "no magenta" | "magenta"
            // * Parsing Lib Recc?
            "no magenta" => resp.votes_against += 1,
            "magenta" => resp.votes_for += 1,
            &_ => {}
        });

    Either::Left(resp)
}

#[derive(Debug)]
pub enum AppError {
    IOError(std::io::Error),
    PostgresError(postgres::Error),
}
type Result<T> = std::result::Result<T, AppError>;

#[actix_web::main]
async fn main() -> Result<()> {
    HttpServer::new(move || App::new().service(votes))
        .bind(("127.0.0.1", 8080))
        .map_err(|e| AppError::IOError(e))?
        .run()
        .await
        .map_err(|e| AppError::IOError(e))
}
