
#[derive(Debug, PartialEq, Eq)]
pub struct QueryResponse {
        pub account: String,
        pub memo: String,
        pub height: i64,
        // pub status: BlockStatus
}

#[derive(Debug, PartialEq, Eq)]
pub struct BlockStatus {
    name: String,
    kind:  BlockStatusType
}

#[derive(Debug, PartialEq, Eq)]
pub struct BlockStatusType {
    oid: String,
    kind: BlockStatusKind,
    schema: String,
}

#[derive(Debug, PartialEq, Eq, )]
pub enum BlockStatusKind {
    Canonical,
    Orphaned,
    Pending
}

impl From<tokio_postgres::Row> for QueryResponse {
    fn from(row: tokio_postgres::Row) -> Self {
    // TODO! DESERIALIZE BlockStatusType
    // println!("{:?}", i);
    
    Self { account: row.get("account"), memo: row.get("memo"), height: row.get("height") }
    }
  }

pub async fn query_database(
    pg_client: &tokio_postgres::Client
) -> Result<(), tokio_postgres::Error> {
  

        for i in pg_client
        .query(QUERY_STATEMENT, &[])
        .await?.into_iter() {
            // TODO! Handle QueryResponse
          QueryResponse::from(i);
        }

        Ok(())
        
}


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