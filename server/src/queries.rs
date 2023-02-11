use crate::types::BlockStatus;
use crate::types::{Vote, VotesCache};
use sqlx::{Pool, Postgres};

use crate::prelude::*;

pub async fn get_latest_blockheight(db: &Pool<Postgres>) -> Result<i64> {
    let height: (i64,) = sqlx::query_as("SELECT MAX(height) FROM blocks;")
        .fetch_one(db)
        .await?;
    Ok(height.0)
}

pub async fn get_signals(
    db: &Pool<Postgres>,
    cache: &VotesCache,
    start: i64,
    end: i64,
    network: crate::types::Network,
) -> Result<Vec<Vote>> {
    if let Some(cached) = cache.get(&f!("{start}-{end}-{network}")) {
        Ok(cached.to_vec())
    } else {
        let votes = sqlx::query_as!(
            Vote,
            r#"
                SELECT DISTINCT pk.value as account, uc.memo as memo, uc.nonce as nonce, uc.hash as hash, b.height as height, b.chain_status as "status: BlockStatus", b.timestamp as timestamp
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
                AND b.timestamp BETWEEN $1 AND $2
            "#, start, end
            ).fetch_all(db).await?;

        Ok(votes)
    }
}
