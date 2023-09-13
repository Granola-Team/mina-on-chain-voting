use diesel::prelude::*;
use diesel::r2d2::ConnectionManager;
use diesel::r2d2::Pool;
use diesel::sql_types::BigInt;
use diesel::sql_types::Text;

use crate::models::diesel::MinaProposal;
use crate::models::vote::ChainStatusType;
use crate::models::vote::MinaBlockStatus;
use crate::prelude::*;

pub(crate) mod cache;
pub(crate) mod mock;
pub(crate) mod postgres;

pub(crate) type PgConnectionPool = Pool<ConnectionManager<PgConnection>>;

pub(crate) trait DBConnectionManager: Send + Sync + 'static {
    fn fetch_chain_tip(&self) -> Result<i64>;
    fn fetch_latest_slot(&self) -> Result<i64>;
    fn fetch_transactions(
        &self,
        start_time: i64,
        end_time: i64,
    ) -> Result<Vec<FetchTransactionResult>>;
    fn fetch_mina_proposals(&self) -> Result<Vec<MinaProposal>>;
    fn fetch_mina_proposal(&self, path: i32) -> Result<MinaProposal>;
}

#[derive(QueryableByName)]
pub(crate) struct FetchChainTipResult {
    #[diesel(sql_type = BigInt)]
    pub(crate) max: i64,
}

#[derive(QueryableByName)]
pub(crate) struct FetchLatestSlotResult {
    #[diesel(sql_type = BigInt)]
    pub(crate) max: i64,
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
