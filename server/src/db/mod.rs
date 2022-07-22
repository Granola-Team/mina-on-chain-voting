use tokio_postgres::{NoTls, config::Config};
use tokio::task::JoinHandle;
use std::sync::Arc;

pub mod queries;

fn init_config() -> tokio_postgres::Config {
    #[cfg(debug_assertions)] // only get dotenv in debug mode
    dotenv::dotenv().ok();

    let dbname = std::env::var("DBNAME").expect("DBNAME not set.");
    let user = std::env::var("USER").expect("PG USER not set.");
    let host = std::env::var("HOST").expect("HOST not set.");
    let password = std::env::var("PASSWD").expect("PG PASSWD not set.");
    let port = str::parse::<u16>(&std::env::var("DBPORT").expect("DBPORT not set.")).expect("Error parsing DBPORT.");

    let mut config = Config::new();
    
    config.dbname(&dbname)
        .user(&user)
        .host(&host)
        .port(port)
        .password(&password);

        config
}

pub async fn connect_to_db() 
-> Result<(JoinHandle<()>, Arc<tokio_postgres::Client>), crate::error::Error> {
    let config = init_config();
    
    let (client, connection) = config
        .connect(NoTls).await?;

    let close_connection = tokio::spawn(async move {
        if let Err(e) = connection.await {
            eprintln!("connection error: {}", e);
        }
    });
    
    Ok((close_connection, std::sync::Arc::new(client)))
}