use tokio_postgres::{NoTls, config::Config};
use tokio::task::JoinHandle;
use std::sync::Arc;

pub mod queries;

pub async fn connect_to_db() 
-> Result<(JoinHandle<()>, Arc<tokio_postgres::Client>), crate::error::Error> {
    #[cfg(debug_assertions)] // only get dotenv in debug mode
    dotenv::dotenv().ok();

    let dbname = std::env::var("DBNAME")?;
    let user = std::env::var("USER")?;
    let host = std::env::var("HOST")?;
    let password = std::env::var("PASSWD")?;
    let port = str::parse::<u16>(&std::env::var("DBPORT")?)?;

    let mut config = Config::new();
    
    config.dbname(&dbname)
        .user(&user)
        .host(&host)
        .port(port)
        .password(&password);

    let (client, connection) = config
        .connect(NoTls).await?;
        
    let close_connection = tokio::spawn(async move {
        if let Err(e) = connection.await {
            eprintln!("connection error: {}", e);
        }
    });
    
    Ok((close_connection, std::sync::Arc::new(client)))
}