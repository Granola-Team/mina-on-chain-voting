use moka::future::Cache as MokaCache;
use std::sync::Arc;

use crate::models::ledger::LedgerAccount;
use crate::models::vote::MinaVote;
use crate::models::vote::MinaVoteWithWeight;

type ArcVotes = Arc<Vec<MinaVote>>;
type ArcVotesWeighted = Arc<Vec<MinaVoteWithWeight>>;
type ArcLedger = Arc<Vec<LedgerAccount>>;

pub(crate) type VotesCache = MokaCache<String, ArcVotes>;
pub(crate) type VotesWeightedCache = MokaCache<String, ArcVotesWeighted>;
pub(crate) type LedgerCache = MokaCache<String, ArcLedger>;

pub(crate) struct CacheManager {
    pub(crate) votes: VotesCache,
    pub(crate) votes_weighted: VotesWeightedCache,
    pub(crate) ledger: LedgerCache,
}

impl CacheManager {
    pub(crate) fn build() -> CacheManager {
        CacheManager {
            votes: VotesCache::builder()
                .time_to_live(std::time::Duration::from_secs(60 * 5))
                .build(),
            votes_weighted: VotesWeightedCache::builder()
                .time_to_live(std::time::Duration::from_secs(60 * 5))
                .build(),
            ledger: LedgerCache::builder()
                .time_to_live(std::time::Duration::from_secs(60 * 60 * 12))
                .build(),
        }
    }
}
