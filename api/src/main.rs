use on_chain_signalling_api::{
    handlers, error::{Result}, db
};

use actix_web::{
    App, HttpServer, web::Data
};

#[actix_web::main]
async fn main() -> Result<()> {
    log4rs::init_file(
        "log4rs.yml", 
        Default::default()
    ).expect("Log4RS.yml file is missing.");

    let (close_db_conn, client) = db::connect_to_db().await?;

    let port = std::env::var("PORT")?.parse::<u16>()?;

    HttpServer::new(move || {
        App::new()
            .app_data(Data::new(client.clone()))
            .service(handlers::votes)

    })
        .bind(("0.0.0.0", port))?
        .run()
        .await?;

    Ok(close_db_conn.await?)
} 
