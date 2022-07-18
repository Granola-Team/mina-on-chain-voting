const QUERY_STATEMENT:
    &'static str = "
        SELECT pk.value as account, uc.memo as memo, b.height as height, b.chain_status as status
        FROM user_commands AS uc
        JOIN blocks_user_commands AS buc
        ON uc.id = buc.user_command_id
        JOIN blocks AS b
        ON buc.block_id = b.id
        JOIN public_keys AS pk
        ON uc.source_id = pk.id
        WHERE uc.type = 'payment'
        AND uc.source_id = uc.receiver_id
        AND uc.token = 1
        AND NOT b.chain_status = 'orphaned'
        AND buc.status = 'applied'
        ;
    ";

pub struct QueryResponse {
    pub account: String,
    pub memo: String,
    pub height: i64,
    pub status: String,
}

pub type VotesMap = std::collections::HashMap<String, (String, i64)>;
pub type APIResponse = Vec<(String, String)>;

pub async fn query_database(
    pg_client: &tokio_postgres::Client
) -> Result<Vec<QueryResponse>, tokio_postgres::Error> {
    pg_client
        .query(QUERY_STATEMENT, &[])
        .await?.iter()
        .map(|row| {
            let account = row.try_get::<&str, String>("account")?;
            let memo = row.try_get::<&str, String>("memo")?;
            let height = row.try_get::<&str, i64>("height")?;
            let status = row.try_get::<&str, String>("status")?;

            Ok(QueryResponse { account, memo, height, status })
        })
        .collect::<Result<Vec<QueryResponse>, tokio_postgres::Error>>()
}

use tokio::task::JoinHandle;
use std::sync::Arc;
pub async fn connect_to_database() 
-> Result<(JoinHandle<()>, Arc<tokio_postgres::Client>), crate::error::Error> {
    #[cfg(debug_assertions)] // only get dotenv in debug mode
    dotenv::dotenv().ok();

    let dbname = std::env::var("DBNAME")?;
    let user = std::env::var("USER")?;
    let host = std::env::var("HOST")?;
    let password = std::env::var("PASSWD")?;
    let port = str::parse::<u16>(&std::env::var("DBPORT")?)?;

    use tokio_postgres::{NoTls, config::Config};
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