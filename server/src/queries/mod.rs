use axum::Extension;
use crate::{models::{DBResponse, BlockStatus}, ApiContext};

pub async fn get_latest_blockheight(ctx: &Extension<ApiContext>) -> Result<i64, sqlx::Error> {
    let row: (i64,) = sqlx::query_as("SELECT MAX(height) FROM blocks;")
    .fetch_one(&ctx.db).await?;
    Ok(row.0)
}

pub async fn get_signals(ctx: &Extension<ApiContext>) -> Result<Vec<DBResponse>, sqlx::Error> {
    let signals = sqlx::query_as!(
        DBResponse,
        // language=PostgreSQL
        r#"
        SELECT pk.value as account, uc.memo as memo, b.height as height, b.chain_status as "status: BlockStatus", b.timestamp as timestamp
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
        "#
    ).fetch_all(&ctx.db).await;
    signals
}
