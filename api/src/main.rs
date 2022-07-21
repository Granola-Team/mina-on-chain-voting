use on_chain_signalling_api::{error::Result, db, routes};

use actix_cors::Cors;
use actix_web::{App, HttpServer, web, middleware};

#[actix_web::main]
async fn main() -> Result<()> {
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));

    let (close_db_conn, client) = db::connect_to_db().await?;
    let port = std::env::var("PORT")?.parse::<u16>()?;

    // TODO! SETUP ACTIX TO SERVE REACT FRONTEND

    HttpServer::new(move || {
        App::new().wrap(
                Cors::default()
                    .allow_any_origin()
                    .allow_any_header()
                    .allow_any_method()
                    .max_age(3600)
                    .send_wildcard(),
            ).wrap(middleware::Logger::default())
            .app_data(web::Data::new(client.clone()))
            .configure(routes::v1_config)

    })
        .bind(("0.0.0.0", port))?
        .run()
        .await?;

    Ok(close_db_conn.await?)
} 
