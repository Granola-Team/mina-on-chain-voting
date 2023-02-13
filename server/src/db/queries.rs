use diesel::sql_types::{BigInt, Text};
use diesel::{sql_query, QueryableByName, RunQueryDsl};
use std::sync::Arc;

use crate::db::cache::CacheManager;
use crate::db::conn::DBConnectionManager;
use crate::mina::vote::*;
use crate::prelude::*;

#[derive(QueryableByName)]
struct FetchChainTipResult {
    #[diesel(sql_type = BigInt)]
    max: i64,
}

pub(crate) async fn fetch_chain_tip(conn_manager: &DBConnectionManager) -> Result<i64> {
    let connection = &mut conn_manager.archive.get().unwrap();
    match sql_query("SELECT MAX(height) FROM blocks").get_result::<FetchChainTipResult>(connection)
    {
        Ok(result) => Ok(result.max),
        Err(_) => Err(Error::Database(f!("failed to fetch current chain-tip."))),
    }
}

#[derive(QueryableByName)]
struct FetchVotesResult {
    #[diesel(sql_type = Text)]
    account: String,
    #[diesel(sql_type = Text)]
    hash: String,
    #[diesel(sql_type = Text)]
    memo: String,
    #[diesel(sql_type = BigInt)]
    height: i64,
    #[diesel(sql_type = ChainStatusType)]
    status: MinaBlockStatus,
    #[diesel(sql_type = BigInt)]
    timestamp: i64,
    #[diesel(sql_type = BigInt)]
    nonce: i64,
}

pub(crate) async fn fetch_votes(
    conn_manager: &DBConnectionManager,
    cache: &CacheManager,
    start: i64,
    end: i64,
) -> Result<Vec<MinaVote>> {
    if let Some(cached) = cache.votes.get(&f!("{start}-{end}")) {
        Ok(cached.to_vec())
    } else {
        let connection = &mut conn_manager.main.get().unwrap();
        return match sql_query(
            f!(
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
            AND b.timestamp BETWEEN {start} AND {end}"
            )
        ).get_results::<FetchVotesResult>(connection) {
            Ok(result) => {
                let votes: Vec<MinaVote> = result.into_iter().map(|res| MinaVote::new(res.account, res.hash,res.memo,res.height,res.status,res.timestamp,res.nonce)).collect();
                cache.votes.insert(f!("{start}-{end}"), Arc::new(votes.clone())).await;
                Ok(votes)
            },
            Err(_) => Err(Error::Database(f!("failed to fetch votes.")))
        };
    }
}
