use serde::{Serialize, Deserialize};

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

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct QueryResponse {  
        pub account: String,
        pub memo: String,
        pub height: i64,
        pub status: BlockStatus
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, FromSql, Serialize, Deserialize)]
#[postgres(name = "chain_status_type")]
pub enum BlockStatus {
    #[postgres(name = "pending")]
    Pending,
    #[postgres(name = "canonical")]
    Canonical,
    #[postgres(name = "orphaned")]
    Orphaned,
}


impl From<tokio_postgres::Row> for QueryResponse {
    fn from(row: tokio_postgres::Row) -> Self {
    Self { account: row.get("account"), memo: row.get("memo"), height: row.get("height"), status: row.get("status") }
    }
}

pub async fn send_query(
    pg_client: &tokio_postgres::Client,
) -> Result<Vec<QueryResponse>, tokio_postgres::Error> {
        pg_client
        .query(QUERY_STATEMENT, &[])
        .await?.into_iter().map(|i| {
            Ok(QueryResponse::from(i))
        }).collect::<Result<Vec<QueryResponse>, tokio_postgres::Error>>()
}


