use std::sync::Arc;
use tokio::task::JoinHandle;
use tokio_postgres::NoTls;

pub mod queries;

pub async fn connect_to_db(
) -> Result<(JoinHandle<()>, Arc<tokio_postgres::Client>), crate::error::Error> {
    let connection_str = std::env::var("DB_CONNECTION_STRING").expect("Environment: DB_CONNECTION_STRING not found.");

    let (client, connection) = tokio_postgres::connect(
        &connection_str,
        NoTls,
      )
      .await?;

    let close_connection = tokio::spawn(async move {
        if let Err(e) = connection.await {
            eprintln!("Connection error: {}", e);
        }
    });

    Ok((close_connection, std::sync::Arc::new(client)))
}
