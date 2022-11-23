use serde::{Deserialize, Serialize};
use sqlx::{FromRow, Type};

use crate::ledger::LedgerDelegations;

/// represents a choice between the Mainnet and Devnet chains
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum QueryRequestFilter {
    /// the current live MINA chain
    Mainnet,
    /// the current live MINA development chain
    Devnet,
}

/// represents a transaction returned by an Archive Node to be processed for signals
#[derive(Debug, PartialEq, Eq, Clone, FromRow, Serialize, Deserialize)]
pub struct DBResponse {
    /// The transaction's associated account
    pub account: String,
    /// The (encoded) memo to be parsed for a key
    pub memo: String,
    /// the block height of the transactions
    pub height: i64,
    /// the block status of the transaction
    pub status: BlockStatus,
    /// the exact time the transaction's block was verified
    pub timestamp: i64,
    /// the relative height of the transaction within its block
    pub nonce: i64
}

/// represents the on-chain status of a transaction
#[derive(Debug, PartialEq, Eq, Clone, Copy, Type, Serialize, Deserialize)]
#[sqlx(rename_all = "lowercase")]
pub enum BlockStatus {
    /// the transaction is potentially on the canonical chain
    Pending,
    /// the transaction is definitely on the canonical chain
    Canonical,
    /// the transaction is definitely not on the canonical chain
    Orphaned,
}

/// A transaction with its memo decoded and ledger delegations and signalling status attached
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct Signal {
    /// the signal's associated account
    pub account: String,
    /// the decoded signalling memo
    pub memo: String,
    /// the block height of the signal
    pub height: i64,
    /// the block status of the signal
    pub status: BlockStatus,
    /// the time the signal's block was verified
    pub timestamp: i64,
    /// the relative height of the signal within the block
    pub nonce: i64,
    /// the amount of delegated MINA to the signal's account and the number of delegators
    pub delegations: LedgerDelegations,
    /// the signalling status of the signal
    pub signal_status: SignalStatus,
}

/// represents the status of a signal related to the current signalling key and block height
#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub enum SignalStatus {
    /// the signal has existed for more blocks than the current settled denominator without being invalidated
    Settled,
    /// the signal is valid but hasn't lasted the settled denominator
    Unsettled,
    /// the signal's memo doesn't match the signalling key or the signal has been shadowed by a more recent one
    Invalid,
}


/// represents the total MINA allocated to yes and no for a MIP
#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize, Default)]
pub struct SignalStats {
    pub yes: f32,
    pub no: f32,
}

/// represents the data sent to the OCS client by this server
#[derive(Debug, PartialEq, Serialize, Deserialize, Default)]
pub struct ResponseEntity {
    /// the processed settled signals
    pub settled: Vec<Signal>,
    /// the processed unsettled signals
    pub unsettled: Vec<Signal>,
    /// all invalidated signals
    pub invalid: Vec<Signal>,
    /// the total signalling results
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
        self.stats = Some(stats);
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
            nonce: 0,
            delegations: LedgerDelegations::default(),
            signal_status: SignalStatus::Settled,
        };

        let signal2 = Signal {
            account: String::from(""),
            memo: String::from(""),
            height: 10,
            status: BlockStatus::Canonical,
            timestamp: 11,
            nonce: 0,
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
