use anyhow::{anyhow, Result};
use graphql_client::*;
use reqwest::IntoUrl;
use serde::de::DeserializeOwned;
use serde::Serialize;

type UInt32 = String;
type UInt64 = String;
type DateTime = String;

pub const MINA_EXPLORER_ENDPOINT: &str = "https://graphql.minaexplorer.com";
pub const DEFAULT_LOCAL_ENDPOINT: &str = "http://localhost:3085/graphql";

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "contrib/explorer_regen_schema.graphql",
    query_path = "contrib/explorer_query.graphql",
    response_derives = "Debug,Serialize,PartialEq"
)]
pub struct StakingDataExplorer;

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "contrib/explorer_regen_schema.graphql",
    query_path = "contrib/explorer_query.graphql",
    response_derives = "Debug,Serialize,PartialEq"
)]
pub struct EpochBlocksWinners;

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "contrib/explorer_regen_schema.graphql",
    query_path = "contrib/explorer_query.graphql",
    response_derives = "Debug,Serialize,PartialEq"
)]
pub struct EpochBlocksForCreator;

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "contrib/explorer_regen_schema.graphql",
    query_path = "contrib/explorer_query.graphql",
    response_derives = "Debug,Serialize,PartialEq"
)]
pub struct BlocksWon;

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "contrib/explorer_regen_schema.graphql",
    query_path = "contrib/explorer_query.graphql",
    response_derives = "Debug,Serialize,PartialEq"
)]
pub struct TransactionsBetweenAddresses;

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "contrib/explorer_regen_schema.graphql",
    query_path = "contrib/explorer_query.graphql",
    response_derives = "Debug,Serialize,PartialEq"
)]
pub struct LastBlockInEpoch;

pub struct LedgerAccount {
    #[allow(unused)]
    pub pk: String,
    pub balance: String,
    pub delegate: String,
    pub index: i64,
}

#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "contrib/regen_schema.graphql",
    query_path = "contrib/query.graphql",
    response_derives = "Debug,Serialize,PartialEq"
)]
pub struct StakingData;

pub async fn graphql_query<U: IntoUrl, B: Serialize + ?Sized, R: DeserializeOwned>(
    endpoint: U,
    request_body: &B,
) -> Result<R> {
    let client = reqwest::Client::new();
    let res = client.post(endpoint).json(request_body).send().await?;
    let response_body: Response<R> = res.json().await?;
    if let Some(es) = response_body.errors {
        for e in es {
            log::error!("{}", e);
        }
        return Err(anyhow!("response_body contains errors"));
    }

    response_body.data.ok_or(anyhow!("response_body was none"))
}
