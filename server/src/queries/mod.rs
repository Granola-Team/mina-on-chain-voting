use std::error::Error;

use crate::{
    models::{BlockStatus, DBResponse},
    router::QueryRequestFilter,
    ApiContext,
};
use axum::Extension;
use sqlx::{Pool, Postgres};

pub async fn get_latest_blockheight(
    ctx: &Extension<ApiContext>,
    network: &QueryRequestFilter,
) -> Result<i64, sqlx::Error> {
    let row: (i64,) = sqlx::query_as("SELECT MAX(height) FROM blocks;")
        .fetch_one(match network {
            QueryRequestFilter::Mainnet => &ctx.mainnet_db,
            _ => &ctx.devnet_db,
        })
        .await?;
    Ok(row.0)
}

/// By default we get all signals every query.
/// This is wildly inefficient & we should work towards an alternative.
/// Since we're essentially only interested in memo's - maybe a way to decode base58 on the DB side?
pub async fn get_signals(
    db: &Pool<Postgres>,
    timestamp: Option<i64>,
) -> Result<Vec<DBResponse>, Box<dyn Error>> {
    use crate::constants::THREE_MONTHS_MILLIS;
    let voting_frontier_timestamp = match timestamp {
        Some(timestamp) => timestamp,
        None =>  std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("SystemTime::now() should not be before the UNIX EPOCH")
            .as_millis() as i64 - THREE_MONTHS_MILLIS, // maybe worry about integer overflow?
    };
    Ok(sqlx::query_as!(
    DBResponse,
    r#"
        SELECT DISTINCT pk.value as account, uc.memo as memo, uc.nonce as nonce, b.height as height, b.chain_status as "status: BlockStatus", b.timestamp as timestamp
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
        AND b.timestamp > $1;
    "#, voting_frontier_timestamp
    ).fetch_all(db).await?)
}
