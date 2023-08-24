use std::collections::HashMap;

use graphql_client::{reqwest::post_graphql_blocking as post_graphql, GraphQLQuery, Response};
use reqwest::blocking::Client;

use crate::transaction_query::TransactionQueryTransactions;

type DateTime = String;

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "src/transaction_schema.graphql",
    query_path = "src/transaction_query.graphql",
    response_derives = "Debug"
)]
pub struct TransactionQuery;

pub(crate) fn fetch_transactions() {
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
