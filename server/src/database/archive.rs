use anyhow::Context;
use chrono::NaiveDateTime;
use chrono::SecondsFormat;
use chrono::Utc;
use diesel::sql_types::{BigInt, Text};
use diesel::{sql_query, QueryableByName, RunQueryDsl};

use crate::database::DBConnectionManager;
use crate::models::vote::{ChainStatusType, MinaBlockStatus};
use crate::prelude::*;

#[derive(QueryableByName)]
pub(crate) struct FetchChainTipResult {
    #[allow(dead_code)]
    #[diesel(sql_type = BigInt)]
    pub(crate) max: i64,
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "src/database/block_schema.graphql",
    query_path = "src/database/block_query.graphql",
    response_derives = "Debug"
)]
pub struct BlockQuery;

#[allow(clippy::unwrap_used, clippy::single_match_else)]
pub(crate) fn fetch_chain_tip() -> i64 {
    let client = Client::new();
    let variables = block_query::Variables {};
    let response_body: Response<block_query::ResponseData> =
        post_graphql::<BlockQuery, _>(&client, "https://graphql.minaexplorer.com", variables)
            .unwrap();

    match response_body.data.unwrap().blocks.first() {
        Some(Some(block_data)) => {
            let block_height = block_data.block_height;
            println!("Chain Tip or Highest Block Height: {block_height:?}");
            block_height.unwrap()
        }
        _ => {
            println!("No blocks found");
            // Return a default value, such as -1, to indicate no blocks found
            -1
        }
    }
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

use std::collections::HashMap;
use std::error;
use std::io;
use std::io::ErrorKind;

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

pub fn encode_memo(s: impl AsRef<[u8]>) -> Result<String, Box<dyn error::Error>> {
    const USER_COMMAND_MEMO: u8 = 0x14;

    const DIGEST_LEN: usize = 32;
    const MAX_INPUT_STRING_LENGTH: usize = DIGEST_LEN;
    const MEMO_LEN: usize = DIGEST_LEN + 2;
    const TAG_INDEX: usize = 0;
    const LEN_INDEX: usize = 1;
    const BYTES_TAG: u8 = 1;
    let s = s.as_ref();
    if s.len() > MAX_INPUT_STRING_LENGTH {
        return Err(Box::new(io::Error::new(ErrorKind::Other, "oh no!")));
    }
    let mut v = vec![0; MEMO_LEN];
    v[TAG_INDEX] = BYTES_TAG;
    v[LEN_INDEX] = s.len() as u8;
    for (i, &b) in s.iter().enumerate() {
        v[i + 2] = b;
    }
    let hash = bs58::encode(v)
        .with_check_version(USER_COMMAND_MEMO)
        .into_string();
    Ok(hash)
}

#[allow(clippy::unwrap_used, clippy::upper_case_acronyms)]
pub(crate) fn fetch_transactions(
    start_time_millis: i64,
    end_time_millis: i64,
    base_memo: &str,
) -> Vec<FetchTransactionResult> {
    let start_dt = NaiveDateTime::from_timestamp_millis(start_time_millis).unwrap();
    let start_time_utc = chrono::DateTime::<Utc>::from_utc(start_dt, Utc);

    let end_dt = NaiveDateTime::from_timestamp_millis(end_time_millis).unwrap();
    let end_time_utc = chrono::DateTime::<Utc>::from_utc(end_dt, Utc);

    let lower_case_memo = base_memo.to_string().to_lowercase();
    let upper_case_memo = base_memo.to_uppercase();
    let no_lower_case_memo = format!("no {}", lower_case_memo);
    let no_upper_case_memo = format!("no {}", upper_case_memo);

    let lower_case_memo_b58 = encode_memo(lower_case_memo.as_bytes()).unwrap();
    let upper_case_memo_b58 = encode_memo(upper_case_memo.as_bytes()).unwrap();
    let no_lower_case_memo_b58 = encode_memo(no_lower_case_memo.as_bytes()).unwrap();
    let no_upper_case_memo_b58 = encode_memo(no_upper_case_memo.as_bytes()).unwrap();

    let variables = transaction_query::Variables {
        date_time_gte: Some(start_time_utc.to_rfc3339_opts(SecondsFormat::Millis, true)),
        date_time_lte: Some(end_time_utc.to_rfc3339_opts(SecondsFormat::Millis, true)),
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
            let timestamp = txn
                .date_time
                .map(|s| match chrono::DateTime::parse_from_rfc3339(&s) {
                    Ok(dt) => dt.timestamp_millis(),
                    Err(_) => 0,
                })
                .unwrap();

            FetchTransactionResult {
                account: txn.to.clone().unwrap(),
                hash: txn.hash.clone().unwrap(),
                memo: txn.memo.clone().unwrap(),
                height: txn.block_height.unwrap(),
                timestamp: timestamp,
                status: MinaBlockStatus::Canonical,
                nonce: txn.nonce.unwrap(),
            }
        })
        .collect::<Vec<FetchTransactionResult>>();
    println!("{:?}", txns.len());
    txns
}
