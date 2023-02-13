use std::io::Write;

use diesel::{
    deserialize::FromSql,
    pg::Pg,
    serialize::{IsNull, ToSql},
    AsExpression, FromSqlRow, SqlType,
};
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};

#[derive(SqlType)]
#[diesel(postgres_type(name = "chain_status_type"))]
pub struct ChainStatusType;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize, FromSqlRow, AsExpression)]
#[diesel(sql_type = ChainStatusType)]
pub enum BlockStatus {
    Pending,
    Canonical,
    Orphaned,
}

impl ToSql<ChainStatusType, Pg> for BlockStatus {
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, Pg>,
    ) -> diesel::serialize::Result {
        match *self {
            BlockStatus::Canonical => out.write_all(b"canonical")?,
            BlockStatus::Pending => out.write_all(b"pending")?,
            BlockStatus::Orphaned => out.write_all(b"orphaned")?,
        }
        Ok(IsNull::No)
    }
}

impl FromSql<ChainStatusType, Pg> for BlockStatus {
    fn from_sql(bytes: diesel::backend::RawValue<'_, Pg>) -> diesel::deserialize::Result<Self> {
        match bytes.as_bytes() {
            b"canonical" => Ok(BlockStatus::Canonical),
            b"pending" => Ok(BlockStatus::Pending),
            b"orphaned" => Ok(BlockStatus::Orphaned),
            _ => Err("unrecognized enum variant".into()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
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

impl Vote {
    pub fn new(
        account: impl Into<String>,
        hash: impl Into<String>,
        memo: impl Into<String>,
        height: i64,
        status: BlockStatus,
        timestamp: i64,
        nonce: i64,
    ) -> Vote {
        Vote {
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
    pub fn new(vote: Vote, stake_weight: Decimal) -> VoteWithWeight {
        VoteWithWeight {
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

pub trait SortByTimestamp {
    fn sort_by_timestamp(self) -> Self;
}

impl SortByTimestamp for Vec<Vote> {
    fn sort_by_timestamp(self) -> Vec<Vote> {
        let mut a = self;
        a.sort_by(|a, b| b.timestamp.cmp(&a.timestamp));
        a
    }
}

impl SortByTimestamp for Vec<VoteWithWeight> {
    fn sort_by_timestamp(self) -> Vec<VoteWithWeight> {
        let mut a = self;
        a.sort_by(|a, b| b.timestamp.cmp(&a.timestamp));
        a
    }
}
