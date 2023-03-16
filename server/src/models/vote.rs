use anyhow::Context;
use diesel::SqlType;
use diesel_derive_enum::DbEnum;
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use std::collections::{hash_map::Entry, HashMap};

use crate::database::archive::FetchTransactionResult;
use crate::models::ledger::Ledger;
use crate::prelude::*;

#[derive(SqlType)]
#[diesel(postgres_type(name = "chain_status_type"))]
pub(crate) struct ChainStatusType;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize, DbEnum)]
#[ExistingTypePath = "ChainStatusType"]
pub(crate) enum MinaBlockStatus {
    Pending,
    Canonical,
    Orphaned,
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub(crate) struct MinaVoteWithWeight {
    pub(crate) account: String,
    pub(crate) hash: String,
    pub(crate) memo: String,
    pub(crate) height: i64,
    pub(crate) status: MinaBlockStatus,
    pub(crate) timestamp: i64,
    pub(crate) nonce: i64,
    pub(crate) weight: Decimal,
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub(crate) struct MinaVote {
    pub(crate) account: String,
    pub(crate) hash: String,
    pub(crate) memo: String,
    pub(crate) height: i64,
    pub(crate) status: MinaBlockStatus,
    pub(crate) timestamp: i64,
    pub(crate) nonce: i64,
}

impl MinaVote {
    pub(crate) fn new(
        account: impl Into<String>,
        hash: impl Into<String>,
        memo: impl Into<String>,
        height: i64,
        status: MinaBlockStatus,
        timestamp: i64,
        nonce: i64,
    ) -> MinaVote {
        MinaVote {
            account: account.into(),
            hash: hash.into(),
            memo: memo.into(),
            height,
            status,
            timestamp,
            nonce,
        }
    }

    pub(crate) fn to_weighted(&self, weight: Decimal) -> MinaVoteWithWeight {
        MinaVoteWithWeight {
            account: self.account.clone(),
            hash: self.hash.clone(),
            memo: self.memo.clone(),
            height: self.height,
            status: self.status,
            timestamp: self.timestamp,
            nonce: self.nonce,
            weight,
        }
    }

    pub(crate) fn update_memo(&mut self, memo: impl Into<String>) {
        let memo = memo.into();
        self.memo = memo;
    }

    pub(crate) fn update_status(&mut self, status: MinaBlockStatus) {
        self.status = status;
    }

    pub(crate) fn is_newer_than(&self, other: &MinaVote) -> bool {
        self.height > other.height || (self.height == other.height && self.nonce > other.nonce)
    }

    pub(crate) fn match_decoded_memo(&mut self, key: &str) -> Option<String> {
        if let Ok(decoded) = self.decode_memo() {
            if decoded.to_lowercase() == key.to_lowercase()
                || decoded.to_lowercase() == f!("no {}", key.to_lowercase())
            {
                return Some(decoded);
            }
        }
        None
    }

    fn decode_memo(&self) -> Result<String> {
        let decoded = bs58::decode(&self.memo)
            .into_vec()
            .with_context(|| f!("failed to decode memo {} - bs58", &self.memo))?;

        let value = &decoded[3..decoded[2] as usize + 3];

        Ok(String::from_utf8(value.to_vec())
            .with_context(|| f!("failed to decode memo {} - from_utf8", &self.memo))?)
    }
}

impl From<FetchTransactionResult> for MinaVote {
    fn from(res: FetchTransactionResult) -> Self {
        MinaVote::new(
            res.account,
            res.hash,
            res.memo,
            res.height,
            res.status,
            res.timestamp,
            res.nonce,
        )
    }
}

impl Wrapper<Vec<MinaVote>> {
    pub(crate) fn process(self, key: impl Into<String>, tip: i64) -> Self {
        let mut map = HashMap::new();
        let key = key.into();

        for mut vote in self.inner() {
            if let Some(memo) = vote.match_decoded_memo(&key) {
                vote.update_memo(memo);

                if tip - vote.height >= 10 {
                    vote.update_status(MinaBlockStatus::Canonical);
                }

                match map.entry(vote.account.clone()) {
                    Entry::Vacant(e) => {
                        e.insert(vote);
                    }
                    Entry::Occupied(mut e) => {
                        let current_vote = e.get_mut();
                        if vote.is_newer_than(current_vote) {
                            *current_vote = vote;
                        }
                    }
                }
            }
        }

        Wrapper(map.values().cloned().collect())
    }

    pub(crate) fn into_weighted(
        self,
        key: impl Into<String>,
        ledger: &Ledger,
        tip: i64,
    ) -> Wrapper<Vec<MinaVoteWithWeight>> {
        let key = key.into();
        let votes = self.process(key, tip);

        let votes_with_stake: Vec<MinaVoteWithWeight> = votes
            .inner()
            .into_iter()
            .filter_map(|vote| {
                let stake = ledger.get_stake_weight(&vote.account).ok()?;
                Some(vote.to_weighted(stake))
            })
            .collect();

        Wrapper(votes_with_stake)
    }

    pub(crate) fn sort_by_timestamp(mut self) -> Self {
        self.0.sort_by(|a, b| b.timestamp.cmp(&a.timestamp));
        self
    }
}

impl Wrapper<Vec<MinaVoteWithWeight>> {
    pub(crate) fn sort_by_timestamp(mut self) -> Self {
        self.0.sort_by(|a, b| b.timestamp.cmp(&a.timestamp));
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn get_test_votes() -> Vec<MinaVote> {
        vec![
            MinaVote::new(
                "1",
                "1",
                "E4YjFkHVUXbEAkQcUrAEcS1fqvbncnn9Tuz2Jtb1Uu79zY9UAJRpd",
                100,
                MinaBlockStatus::Pending,
                100,
                1,
            ),
            MinaVote::new(
                "1",
                "2",
                "E4YjFkHVUXbEAkQcUrAEcS1fqvbncnn9Tuz2Jtb1Uu79zY9UAJRpd",
                110,
                MinaBlockStatus::Pending,
                110,
                1,
            ),
            MinaVote::new(
                "2",
                "3",
                "E4YjFkHVUXbEAkQcUrAEcS1fqvbncnn9Tuz2Jtb1Uu79zY9UAJRpd",
                110,
                MinaBlockStatus::Pending,
                110,
                1,
            ),
            MinaVote::new(
                "2",
                "4",
                "E4YdLeukpqzqyBAxujeELx9SZWoUW9MhcUfnGHF9PhQmxTJcpmj7j",
                120,
                MinaBlockStatus::Pending,
                120,
                2,
            ),
            MinaVote::new(
                "2",
                "4",
                "E4YiC7vB4DC9JoQvaj83nBWwHC3gJh4G9EBef7xh4ti4idBAgZai7",
                120,
                MinaBlockStatus::Pending,
                120,
                2,
            ),
        ]
    }

    #[test]
    fn test_decode_memo() {
        let mut vote = MinaVote::new("1", "1", "", 100, MinaBlockStatus::Pending, 100, 1);

        vote.update_memo("E4Yf92G48v8FApR4EWQq3iKb2vZkHHxZHPaZ73NQNBXmHeXNzHHSp");
        assert_eq!(vote.decode_memo().unwrap(), "Payment#0");

        vote.update_memo("E4YjFkHVUXbEAkQcUrAEcS1fqvbncnn9Tuz2Jtb1Uu79zY9UAJRpd");
        assert_eq!(vote.decode_memo().unwrap(), "no cftest-2");

        vote.update_memo("E4ZJ3rmurwsMFrSvLdSAGRqmXRjYeZt84Wws4dixfpN67Xj7SrRLu");
        assert_eq!(vote.decode_memo().unwrap(), "MinaExplorer Gas Fee Service");

        vote.update_memo("E4YM2vTHhWEg66xpj52JErHUBU4pZ1yageL4TVDDpTTSsv8mK6YaH");
        assert_eq!(vote.decode_memo().unwrap(), "");
    }

    #[test]
    fn test_match_decode_memo() {
        let key = "cftest-2";
        let mut votes = get_test_votes();

        let v1_decoded = votes[0].match_decoded_memo(key).unwrap();
        let v2_decoded = votes[1].match_decoded_memo(key).unwrap();
        let v3_decoded = votes[2].match_decoded_memo(key).unwrap();
        let v4_decoded = votes[3].match_decoded_memo(key).unwrap();
        let v5_decoded = votes[4].match_decoded_memo(key);

        assert_eq!(v1_decoded, "no cftest-2");
        assert_eq!(v2_decoded, "no cftest-2");
        assert_eq!(v3_decoded, "no cftest-2");
        assert_eq!(v4_decoded, "cftest-2");
        assert_eq!(v5_decoded, None);
    }

    #[test]
    fn test_process_votes() {
        let votes = get_test_votes();
        let processed = Wrapper(votes).process("cftest-2", 129).inner();

        assert_eq!(processed.len(), 2);

        let a1 = processed.iter().find(|s| s.account == "1").unwrap();
        let a2 = processed.iter().find(|s| s.account == "2").unwrap();

        assert_eq!(a1.account, "1");
        assert_eq!(a1.hash, "2");
        assert_eq!(a1.memo, "no cftest-2");
        assert_eq!(a1.height, 110);
        assert_eq!(a1.status, MinaBlockStatus::Canonical);
        assert_eq!(a1.nonce, 1);

        assert_eq!(a2.account, "2");
        assert_eq!(a2.hash, "4");
        assert_eq!(a2.memo, "cftest-2");
        assert_eq!(a2.height, 120);
        assert_eq!(a2.status, MinaBlockStatus::Pending);
        assert_eq!(a2.nonce, 2);
    }
}
