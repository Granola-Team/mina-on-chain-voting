use on_chain_signalling_api::{error::Result, db, routes};

use actix_web::{App, HttpServer, web, middleware};
use actix_files as fs;
use actix_cors::Cors;

#[actix_web::main]
async fn main() -> Result<()> {
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));

    let (close_db_conn, client) = db::connect_to_db().await?;
    let port = std::env::var("PORT")?.parse::<u16>()?;


    HttpServer::new(move || {
        App::new().wrap(
                Cors::default()
                    .allow_any_origin()
                    .allow_any_header()
                    .allowed_methods(vec!["GET"])
                    .max_age(3600)
                    .send_wildcard(),
            ).wrap(middleware::Logger::default())
            .app_data(web::Data::new(client.clone())).service(web::scope("/api").configure(routes::v1_config)).service(fs::Files::new("/", "./build").prefer_utf8(true).index_file("index.html"))
            

    })
        .bind(("0.0.0.0", port))?
        .run()
        .await?;

    Ok(close_db_conn.await?)
} 
