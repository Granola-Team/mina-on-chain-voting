use crate::signal::{BlockStatus, Signal, SignalCache};
use sqlx::{Pool, Postgres};

pub async fn get_latest_blockheight(db: &Pool<Postgres>) -> anyhow::Result<i64> {
    let height: (i64,) = sqlx::query_as("SELECT MAX(height) FROM blocks;")
        .fetch_one(db)
        .await?;
    Ok(height.0)
}

pub async fn get_signals(
    db: &Pool<Postgres>,
    cache: &SignalCache,
    start: i64,
    end: i64,
    network: crate::types::Network,
) -> anyhow::Result<Vec<Signal>> {
    if let Some(cached) = cache.get(&format!("{}-{}-{}", start, end, network)) {
        Ok(cached.to_vec())
    } else {
        let signals = sqlx::query_as!(
            Signal,
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

        // Temp whitelist for MIP01 period
        if start == 1672848000000 && end == 1673685000000 {
            cache
                .insert(
                    format!("{}-{}-{}", start, end, network),
                    std::sync::Arc::new(signals.clone()),
                )
                .await;
        }

        Ok(signals)
    }
}
