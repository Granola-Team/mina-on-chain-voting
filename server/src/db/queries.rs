use crate::models::DBResponse;

const QUERY_STATEMENT:
    &'static str = "
        SELECT pk.value as account, uc.memo as memo, b.height as height, b.chain_status as status, b.timestamp as timestamp
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

impl From<tokio_postgres::Row> for DBResponse {
        fn from(row: tokio_postgres::Row) -> Self {
        Self { account: row.get("account"), memo: row.get("memo"), height: row.get("height"), status: row.get("status"), timestamp: row.get("timestamp"), signal_status: None}
        }
}


pub async fn get_latest_blockheight(pg_client: &tokio_postgres::Client) -> Result<i64, tokio_postgres::Error> {
    let row = pg_client.query_one("SELECT MAX(height) FROM blocks;", &[]).await?;
    Ok(row.get(0))
}

pub async fn get_memo_data(
    pg_client: &tokio_postgres::Client,
) -> Result<Vec<DBResponse>, tokio_postgres::Error> {
        pg_client
        .query(QUERY_STATEMENT, &[])
        .await?.into_iter().map(|i| {
            Ok(DBResponse::from(i))
        }).collect::<Result<Vec<DBResponse>, tokio_postgres::Error>>()
}


