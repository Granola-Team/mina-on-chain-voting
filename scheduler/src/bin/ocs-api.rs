extern crate serde_derive;

use on_chain_signalling_api::{
    handlers, error::Error, database::connect_to_database
};

use actix_web::{
    App, HttpServer
};

type Result<T> = std::result::Result<T, Error>;

#[actix_web::main]
async fn main() -> Result<()> {
    log4rs::init_file(
        "log4rs.yml", 
        Default::default()
    ).unwrap();

    let (close_db_conn, client) = connect_to_database().await?;

    let port = str::parse::<u16>(&std::env::var("PORT")?)?;

    HttpServer::new(move || {
        App::new()
            .app_data(client.clone())
            .service(handlers::votes)

    })
        .bind(("0.0.0.0", port))?
        .run()
        .await?;

    Ok(close_db_conn.await?)
} 
