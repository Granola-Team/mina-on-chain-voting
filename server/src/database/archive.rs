use anyhow::Context;
use diesel::sql_types::{BigInt, Text};
use diesel::{sql_query, QueryableByName, RunQueryDsl};

use crate::database::DBConnectionManager;
use crate::models::vote::{ChainStatusType, MinaBlockStatus};
use crate::prelude::*;
use time::{OffsetDateTime, UtcOffset};

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
        "SELECT DISTINCT pk.value as account, uc.memo as memo, uc.nonce as nonce, uc.hash as hash, b.height as height, b.chain_status as status, b.timestamp as timestamp
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
        AND b.timestamp BETWEEN $1 AND $2"
        );
    let results = results
        .bind::<BigInt, _>(start_time)
        .bind::<BigInt, _>(end_time)
        .get_results(connection)?;
    Ok(results)
}

use std::collections::HashMap;
use std::time::Duration;

use graphql_client::{reqwest::post_graphql_blocking as post_graphql, GraphQLQuery, Response};
use reqwest::blocking::Client;

use transaction_query::TransactionQueryTransactions;

type DateTime = String;

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "src/database/transaction_schema.graphql",
    query_path = "src/database/transaction_query.graphql",
    response_derives = "Debug"
)]
pub struct TransactionQuery;

#[allow(dead_code, clippy::unwrap_used, clippy::upper_case_acronyms)]
pub(crate) fn fetch_transactions_graphql(
    start_time_millis: i64,
    end_time_millis: i64,
    _mip_key: &str,
    base_memo: &str,
) -> Vec<FetchTransactionResult> {
    let start_duration = Duration::from_millis(start_time_millis.try_into().unwrap());
    let end_duration = Duration::from_millis(end_time_millis.try_into().unwrap());

    let start_datetime = OffsetDateTime::UNIX_EPOCH + start_duration;
    let end_datetime = OffsetDateTime::UNIX_EPOCH + end_duration;

    let start_utc_datetime = start_datetime.to_offset(UtcOffset::UTC);
    let end_utc_datetime = end_datetime.to_offset(UtcOffset::UTC);

    let lower_case_memo = base_memo.to_string();
    let upper_case_memo = base_memo.to_uppercase();
    let no_lower_case_memo = format!("{upper_case_memo:?}");
    let no_upper_case_memo = format!("NO {upper_case_memo:?}");

    let lower_case_memo_b58 = bs58::encode(lower_case_memo).into_string();
    let upper_case_memo_b58 = bs58::encode(upper_case_memo).into_string();
    let no_lower_case_memo_b58 = bs58::encode(no_lower_case_memo).into_string();
    let no_upper_case_memo_b58 = bs58::encode(no_upper_case_memo).into_string();


    let variables = transaction_query::Variables {
        date_time_gte: Some(start_utc_datetime.to_string()),
        date_time_lte: Some(end_utc_datetime.to_string()),
        memo1: Some(lower_case_memo_b58),
        memo2: Some(upper_case_memo_b58),
        memo3: Some(no_lower_case_memo_b58),
        memo4: Some(no_upper_case_memo_b58),
    };
    let client = Client::new();
    let response_body: Response<transaction_query::ResponseData> =
        post_graphql::<TransactionQuery, _>(&client, "https://graphql.minaexplorer.com", variables)
            .unwrap();

    let txns = response_body.data.unwrap().transactions;

    let txns = txns
        .into_iter()
        .flatten()
        .filter(|tx| tx.to.clone().unwrap() == tx.from.clone().unwrap())
        .fold(HashMap::new(), |mut map, txn| {
            // Dedup based on from public key keeping the most recent transaction
            map.entry(txn.from.clone().unwrap()).or_insert(txn);
            map
        })
        .into_values()
        .map(|txn: TransactionQueryTransactions| {
            let timestamp_seconds = txn.date_time.expect("not a valid time value").parse::<i64>().unwrap_or(0);
            let offset_datetime = OffsetDateTime::from_unix_timestamp(timestamp_seconds);

            FetchTransactionResult {
                account: txn.to.clone().unwrap(),
                hash: txn.hash.clone().unwrap(),
                memo: txn.memo.clone().unwrap(),
                height: txn.block_height.unwrap(),
                timestamp: offset_datetime.expect("not a valid time value").unix_timestamp(),
                status: MinaBlockStatus::Canonical,
                nonce: txn.nonce.unwrap(),
            }
        })
        .collect::<Vec<FetchTransactionResult>>();
    println!("{:?}", txns.len());
    txns
}
