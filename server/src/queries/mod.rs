use std::{error::Error, ops::Sub, time::Duration};

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
    let three_months_ago: i64 = std::time::SystemTime::now()
        .sub(Duration::from_secs(2629800 * 3))
        .duration_since(std::time::UNIX_EPOCH)?
        .as_secs() as i64;

    Ok(sqlx::query_as!(DBResponse,
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
    AND b.timestamp > $1
    "#, match timestamp {
        Some(timestamp) => timestamp,
    None => three_months_ago
    }
    ).fetch_all(db).await?)
}
