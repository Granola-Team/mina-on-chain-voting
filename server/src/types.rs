use moka::future::Cache;
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use sqlx::{FromRow, Type};
use std::fmt::Display;

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Network {
    Mainnet,
}

impl Display for Network {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Network::Mainnet => write!(f, "mainnet"),
        }
    }
}

pub type VotesCache = Cache<String, std::sync::Arc<Vec<Vote>>>;
pub type LedgerCache = Cache<String, std::sync::Arc<bytes::Bytes>>;

#[derive(Debug, PartialEq, Eq, Clone, FromRow, Serialize, Deserialize)]

pub struct Vote {
    pub account: String,
    pub hash: String,
    pub memo: String,
    pub height: i64,
    pub status: BlockStatus,
    pub timestamp: i64,
    pub nonce: i64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VoteWithWeight {
    pub account: String,
    pub hash: String,
    pub memo: String,
    pub height: i64,
    pub status: BlockStatus,
    pub timestamp: i64,
    pub nonce: i64,
    pub stake_weight: Decimal,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Type, Serialize, Deserialize)]
#[sqlx(rename_all = "lowercase")]
pub enum BlockStatus {
    Pending,
    Canonical,
    Orphaned,
}

impl Vote {
    pub fn new(
        account: impl Into<String>,
        hash: impl Into<String>,
        memo: impl Into<String>,
        height: i64,
        status: BlockStatus,
        timestamp: i64,
        nonce: i64,
    ) -> Self {
        Self {
            account: account.into(),
            hash: hash.into(),
            memo: memo.into(),
            height,
            status,
            timestamp,
            nonce,
        }
    }
}

impl VoteWithWeight {
    pub fn new(vote: Vote, stake_weight: Decimal) -> Self {
        Self {
            account: vote.account,
            hash: vote.hash,
            memo: vote.memo,
            height: vote.height,
            status: vote.status,
            timestamp: vote.timestamp,
            nonce: vote.nonce,
            stake_weight,
        }
    }
}

pub trait VoteExt {
    fn update_memo(&mut self, memo: String);
    fn mark_canonical(&mut self);
}

impl VoteExt for Vote {
    fn update_memo(&mut self, memo: String) {
        self.memo = memo;
    }

    fn mark_canonical(&mut self) {
        self.status = BlockStatus::Canonical;
    }
}

impl VoteExt for VoteWithWeight {
    fn update_memo(&mut self, memo: String) {
        self.memo = memo;
    }

    fn mark_canonical(&mut self) {
        self.status = BlockStatus::Canonical;
    }
}
