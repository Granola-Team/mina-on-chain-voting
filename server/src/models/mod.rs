use serde::{Deserialize, Serialize};
use sqlx::{FromRow, Type};

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
    pub delegations: LedgerDelegations,
    pub signal_status: SignalStatus,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub enum SignalStatus {
    Settled,
    Unsettled,
    Invalid,
}

#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize, Default)]
pub struct SignalStats {
    pub yes: f32,
    pub no: f32,
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Default)]
pub struct ResponseEntity {
    pub settled: Vec<Signal>,
    pub unsettled: Vec<Signal>,
    pub invalid: Vec<Signal>,
    pub stats: Option<SignalStats>,
}

impl ResponseEntity {
    pub fn new(settled: Vec<Signal>, unsettled: Vec<Signal>, invalid: Vec<Signal>) -> Self {
        Self {
            settled,
            unsettled,
            invalid,
            stats: Default::default(),
        }
    }

    pub fn sort(mut self) -> Self {
        self.settled.sort_by(|a, b| b.height.cmp(&a.height));
        self.unsettled.sort_by(|a, b| b.height.cmp(&a.height));
        self.invalid.sort_by(|a, b| b.height.cmp(&a.height));
        self
    }

    pub fn with_stats(mut self, stats: SignalStats) -> Self {
        self.stats = stats;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn response_entity_sort_sorts_signals_by_height() {
        let signal1 = Signal {
            account: String::from(""),
            memo: String::from(""),
            height: 8,
            status: BlockStatus::Canonical,
            timestamp: 10,
            delegations: LedgerDelegations::default(),
            signal_status: SignalStatus::Settled,
        };

        let signal2 = Signal {
            account: String::from(""),
            memo: String::from(""),
            height: 10,
            status: BlockStatus::Canonical,
            timestamp: 11,
            delegations: LedgerDelegations::default(),
            signal_status: SignalStatus::Settled,
        };

        let response_entity = ResponseEntity {
            settled: vec![signal1, signal2.clone()],
            unsettled: vec![],
            invalid: vec![],
            stats: Default::default(),
        };
        let result = response_entity.sort();
        assert_eq!(result.settled[0], signal2);
    }

    #[test]
    fn response_entity_with_stats_adds_stats() {
        let response_entity = ResponseEntity::default();
        let stats = SignalStats { yes: 100., no: 50. };
        let with_stats = response_entity.with_stats(stats);
        assert_eq!(stats, with_stats.stats.unwrap());
    }
}
