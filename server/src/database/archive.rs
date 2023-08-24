use std::collections::HashMap;

use graphql_client::{reqwest::post_graphql_blocking as post_graphql, GraphQLQuery, Response};
use reqwest::blocking::Client;

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

use transaction_query::TransactionQueryTransactions;

type DateTime = String;

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "src/database/transaction_schema.graphql",
    query_path = "src/database/transaction_query.graphql",
    response_derives = "Debug"
)]
pub struct TransactionQuery;

#[allow(dead_code)]
pub(crate) fn fetch_transactions_graphql() {
    let variables = transaction_query::Variables {
        date_time_gte: Some("2023-05-20T06:00:00Z".to_string()),
        date_time_lte: Some("2023-05-28T06:00:00Z".to_string()),
        memo1: Some("E4YVPwLUR2LrP9tSSi3fjw1svcZys1gJHrGvRefwVTCMbP2NQRqdW".to_string()),
        memo2: Some("E4YVe5wRALCJJ2dGEwRMyH7Z8y1QxzM76M8rXivFo5XbeBJdKryV6".to_string()),
        memo3: Some("E4YbUmaZjNgLgezBD3JzyGKuCn4iugZ5EcXT1JuNTudm5tT4MHvKz".to_string()),
        memo4: Some("E4YbUmaZZqAoUdTZYvZkSmLjHfccTMbb5RnTQHixwRWq2YqLdLZyE".to_string()),
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
        .collect::<Vec<TransactionQueryTransactions>>();
    println!("{:?}", txns.len());
}
