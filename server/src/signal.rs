use moka::future::Cache;
use serde::{Deserialize, Serialize};
use sqlx::{FromRow, Type};

pub type SignalCache = Cache<String, std::sync::Arc<Vec<Signal>>>;

#[derive(Debug, PartialEq, Eq, Clone, FromRow, Serialize, Deserialize)]
pub struct Signal {
    pub account: String,
    pub memo: String,
    pub height: i64,
    pub status: BlockStatus,
    pub timestamp: i64,
    pub nonce: i64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SignalWithStake {
    pub account: String,
    pub memo: String,
    pub height: i64,
    pub status: BlockStatus,
    pub timestamp: i64,
    pub nonce: i64,
    pub stake: Stake,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Stake {
    pub total_delegators: i32,
    pub delegated_balance: f64,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Type, Serialize, Deserialize)]
#[sqlx(rename_all = "lowercase")]
pub enum BlockStatus {
    Pending,
    Canonical,
    Orphaned,
}

impl SignalWithStake {
    pub fn new(signal: Signal, stake: Stake) -> Self {
        Self {
            account: signal.account,
            memo: signal.memo,
            height: signal.height,
            status: signal.status,
            timestamp: signal.timestamp,
            nonce: signal.nonce,
            stake,
        }
    }
}

pub trait SignalExt {
    fn update_memo(&mut self, memo: String);
}

impl SignalExt for Signal {
    fn update_memo(&mut self, memo: String) {
        self.memo = memo;
    }
}

impl SignalExt for SignalWithStake {
    fn update_memo(&mut self, memo: String) {
        self.memo = memo;
    }
}
