use serde::{Deserialize, Serialize};
use sqlx::{Type, FromRow};

use crate::ledger::LedgerDelegations;

#[derive(Debug, PartialEq, Eq, Clone, FromRow, Serialize, Deserialize)]
pub struct DBResponse {
    pub account: String,
    pub memo: String,
    pub height: i64,
    pub status: BlockStatus,
    pub timestamp: i64,
}


#[derive(Debug, PartialEq, Eq, Clone, Copy, Type, Serialize, Deserialize)]
#[sqlx(rename_all = "lowercase")]
pub enum BlockStatus {
    Pending,
    Canonical,
    Orphaned,
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct Signal {
    pub account: String,
    pub memo: String,
    pub height: i64,
    pub status: BlockStatus,
    pub timestamp: i64,
    pub delegations: Option<LedgerDelegations>,
    pub signal_status: Option<SignalStatus>
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub enum SignalStatus {
    Settled,
    Unsettled,
    Invalid,
}


#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub struct SignalStats {
    pub yes: f32,
    pub no: f32
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ResponseEntity {
    pub signals: Vec<Signal>,
    pub stats: Option<SignalStats>,
}

impl ResponseEntity {
    pub fn new(signals: Vec<Signal>) -> Self {
        Self { signals, stats: None }
    }

    pub fn sort(mut self) -> Self {
        self.signals.sort_by(|a, b| b.height.cmp(&a.height));
        self
    }

    pub fn with_stats(mut self, stats: SignalStats) -> Self {
        self.stats = Some(stats);
        self
    }
}

