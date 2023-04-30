use anyhow::Context;
use diesel::sql_types::{BigInt, Text};
use diesel::{sql_query, QueryableByName, RunQueryDsl};

use crate::database::DBConnectionManager;
use crate::models::vote::{ChainStatusType, MinaBlockStatus};
use crate::prelude::*;

#[derive(QueryableByName)]
pub(crate) struct FetchChainTipResult {
    #[diesel(sql_type = BigInt)]
    pub(crate) max: i64,
}

pub(crate) fn fetch_chain_tip(conn_manager: &DBConnectionManager) -> Result<i64> {
    let connection = &mut conn_manager
        .archive
        .get()
        .context("failed to get archive db connection")?;

    let result = sql_query("SELECT MAX(height) FROM blocks")
        .get_result::<FetchChainTipResult>(connection)?;
    Ok(result.max)
}

#[derive(QueryableByName)]
pub(crate) struct FetchLatestSlotResult {
    #[diesel(sql_type = BigInt)]
    pub(crate) max: i64,
}

pub(crate) fn fetch_latest_slot(conn_manager: &DBConnectionManager) -> Result<i64> {
    let connection = &mut conn_manager
        .archive
        .get()
        .context("failed to get archive db connection")?;

    let result = sql_query("SELECT MAX(global_slot) FROM blocks")
        .get_result::<FetchLatestSlotResult>(connection)?;
    Ok(result.max)
}

#[derive(QueryableByName)]
pub(crate) struct FetchTransactionResult {
    #[diesel(sql_type = Text)]
    pub(crate) account: String,
    #[diesel(sql_type = Text)]
    pub(crate) hash: String,
    #[diesel(sql_type = Text)]
    pub(crate) memo: String,
    #[diesel(sql_type = BigInt)]
    pub(crate) height: i64,
    #[diesel(sql_type = ChainStatusType)]
    pub(crate) status: MinaBlockStatus,
    #[diesel(sql_type = BigInt)]
    pub(crate) timestamp: i64,
    #[diesel(sql_type = BigInt)]
    pub(crate) nonce: i64,
}

pub(crate) fn fetch_transactions(
    conn_manager: &DBConnectionManager,
    start_time: i64,
    end_time: i64,
) -> Result<Vec<FetchTransactionResult>> {
    let connection = &mut conn_manager
        .archive
        .get()
        .context("failed to get archive db connection")?;

    let results = sql_query(
        "SELECT DISTINCT pk.value as account, uc.memo as memo, uc.nonce as nonce, uc.hash as hash, b.height as height, b.chain_status as status, b.timestamp::bigint as timestamp
        FROM user_commands AS uc
        JOIN blocks_user_commands AS buc
        ON uc.id = buc.user_command_id
        JOIN blocks AS b
        ON buc.block_id = b.id
        JOIN public_keys AS pk
        ON uc.source_id = pk.id
        WHERE uc.command_type = 'payment'
        AND uc.source_id = uc.receiver_id
        AND NOT b.chain_status = 'orphaned'
        AND buc.status = 'applied'
        AND b.timestamp::bigint BETWEEN $1 AND $2"
        );
    let results = results
        .bind::<BigInt, _>(start_time)
        .bind::<BigInt, _>(end_time)
        .get_results(connection)?;
    Ok(results)
}
