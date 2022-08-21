use on_chain_signalling_api::{db, error::Result, routes, ledger::Ledger};
use actix_web::{middleware, web, App, HttpServer};
use actix_cors::Cors;

extern crate dotenv;

#[actix_web::main]
async fn main() -> Result<()> {
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));
    dotenv::dotenv().ok();

    let ledger = Ledger::init();
    ledger.migrate();

    let (close_db_conn, client) = db::connect_to_db().await?;

    HttpServer::new(move || {
        App::new()
            .wrap(
                Cors::default()
                    .allow_any_origin()
                    .allow_any_header()
                    .allowed_methods(vec!["GET"])
                    .max_age(3600)
                    .send_wildcard(),
            )
            .wrap(middleware::Logger::default())
            .app_data(web::Data::new(client.clone()))
            .service(web::scope("/api").configure(routes::v1_config))
    })
    .bind(("0.0.0.0", 8080))?
    .run()
    .await?;

    Ok(close_db_conn.await?)
}
