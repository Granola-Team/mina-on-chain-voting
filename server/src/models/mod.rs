use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Eq, Clone, FromSql, Serialize, Deserialize)]
pub struct DBResponse {
    pub account: String,
    pub memo: String,
    pub height: i64,
    pub status: BlockStatus,
    pub signal_status: Option<Status>,
    pub timestamp: i64,
    pub delegations: Option<crate::ledger::LedgerDelegations>
}


#[derive(Debug, PartialEq, Eq, Clone, Copy, FromSql, Serialize, Deserialize)]
#[postgres(name = "chain_status_type")]
pub enum BlockStatus {
    #[postgres(name = "pending")]
    Pending,
    #[postgres(name = "canonical")]
    Canonical,
    #[postgres(name = "orphaned")]
    Orphaned,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub struct SignalStats {
    pub yes: i32,
    pub no: i32
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ResponseEntity {
    pub signals: Vec<DBResponse>,
    pub stats: Option<SignalStats>,
}

impl ResponseEntity {
    pub fn new(signals: Vec<DBResponse>) -> Self {
        Self { signals: signals.into_iter().filter(|x| x.delegations.is_some()).collect(), stats: None }
    }

    pub fn sorted(mut self, sorted: Option<bool>) -> Self {
        if let Some(b) = sorted {
            if b {
                self.signals.sort_by(|a, b| b.height.cmp(&a.height));
                return self;
            } else {
                return self;
            }
        }
        self
    }

    pub fn with_stats(mut self, s: Option<bool>, stats: Option<SignalStats>) -> Self {
        if let Some(b) = s {
            if b && stats.is_some() {
                self.stats = stats
            }
        }
        self
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, FromSql, Serialize, Deserialize)]
pub enum Status {
    Settled,
    Unsettled,
    Invalid,
}
