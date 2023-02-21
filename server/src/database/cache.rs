use moka::future::Cache as MokaCache;
use std::sync::Arc;

use crate::models::ledger::LedgerAccount;
use crate::models::vote::MinaVote;

// TODO: change cache key to be for mips instead keyword
type ArcVotes = Arc<Vec<MinaVote>>;
type ArcLedger = Arc<Vec<LedgerAccount>>;

pub(crate) type VotesCache = MokaCache<String, ArcVotes>;
pub(crate) type LedgerCache = MokaCache<String, ArcLedger>;

pub(crate) struct CacheManager {
    pub(crate) votes: VotesCache,
    pub(crate) ledger: LedgerCache,
}

impl CacheManager {
    pub(crate) fn build() -> CacheManager {
        CacheManager {
            votes: VotesCache::builder()
                .time_to_live(std::time::Duration::from_secs(60 * 5))
                .build(),
            ledger: LedgerCache::builder()
                .time_to_live(std::time::Duration::from_secs(60 * 60 * 12))
                .build(),
        }
    }
}
