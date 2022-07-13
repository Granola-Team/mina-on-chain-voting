extern crate serde_derive;

use on_chain_signalling_api::{
    handlers, error::Error, database::connect_to_database
};

use actix_web::{
    App, HttpServer, web::Data,
};

use actix_cors::Cors; 

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
            .wrap(Cors::default())    
            .app_data(Data::new(client.clone()))
            .service(handlers::votes);
    })
        .bind(("35.203.38.140", "8080/votes"))?
        .run()
        .await?;
        
    Ok(close_db_conn.await?)
} // .bind use to be .bind(("127.0.0.1", port))?  => "35.203.38.140", "8080/votes"
// added .wrap(Cors::default()) 
// use actix_cors::Cors; 
