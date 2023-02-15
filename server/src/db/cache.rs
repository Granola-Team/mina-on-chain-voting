use moka::future::Cache as MokaCache;
use std::sync::Arc;

use crate::mina::vote::MinaVote;

type ArcVotes = Arc<Vec<MinaVote>>;
type ArcBytes = Arc<bytes::Bytes>;

pub(crate) type VotesCache = MokaCache<String, ArcVotes>;
pub(crate) type LedgerCache = MokaCache<String, ArcBytes>;

pub(crate) struct CacheManager {
    pub(crate) votes: VotesCache,
    pub(crate) ledger: LedgerCache,
}

impl CacheManager {
    pub(crate) fn build() -> CacheManager {
        CacheManager {
            votes: VotesCache::builder()
                .time_to_live(std::time::Duration::from_secs(60 * 3))
                .build(),
            ledger: LedgerCache::builder()
                .time_to_live(std::time::Duration::from_secs(60 * 60 * 12))
                .build(),
        }
    }
}
