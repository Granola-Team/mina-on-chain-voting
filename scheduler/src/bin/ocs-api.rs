extern crate serde_derive;

use on_chain_signalling_api::handlers;

use actix_web::{
    App, HttpServer, web::Data,
};

use std::env;

#[derive(Debug)]
enum Error {
    IOError(std::io::Error),
    EnvVarError(env::VarError),
    PostgresError(tokio_postgres::Error),
}
type Result<T> = std::result::Result<T, Error>;

#[actix_web::main]
async fn main() -> Result<()> {
    dotenv::dotenv().ok();

    let dbname = env::var("DBNAME")?;
    let user = env::var("USER")?;
    let host = env::var("HOST")?;

    use tokio_postgres::{NoTls, config::Config};
    let mut config = Config::new();
    config.dbname(&dbname);
    config.user(&user);
    config.host(&host);
    let (client, connection) = config
        .connect(NoTls).await?;
        
    tokio::spawn(async move {
        if let Err(e) = connection.await {
            eprintln!("connection error: {}", e);
        }
    });
    let arc_client = std::sync::Arc::new(client);

    Ok(HttpServer::new(move || {
        App::new()
            .app_data(Data::new(arc_client.clone()))
            .service(handlers::votes)
    })
        .bind(("127.0.0.1", 8080))?
        .run()
        .await?)
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Error::IOError(err)
    }
}

impl From<std::env::VarError> for Error {
    fn from(err: std::env::VarError) -> Self {
        Error::EnvVarError(err)
    }
}

impl From<tokio_postgres::Error> for Error {
    fn from(err: tokio_postgres::Error) -> Self {
        Error::PostgresError(err)
    }
}