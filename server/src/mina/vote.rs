use diesel::SqlType;
use diesel_derive_enum::DbEnum;
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use std::collections::{hash_map::Entry, HashMap};

use crate::mina::ledger::get_stake_weight;
use crate::mina::ledger::Ledger;
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
pub(crate) struct MinaVote {
    pub account: String,
    pub hash: String,
    pub memo: String,
    pub height: i64,
    pub status: MinaBlockStatus,
    pub timestamp: i64,
    pub nonce: i64,
    pub stake_weight: Option<Decimal>,
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
            stake_weight: None,
        }
    }

    pub(crate) fn update_stake(&mut self, stake_weight: Decimal) {
        self.stake_weight = Some(stake_weight);
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
            if decoded.to_lowercase().contains(&key.to_lowercase()) {
                self.update_memo(&decoded);
                if self.match_memo(&decoded) {
                    return Some(decoded.to_string());
                }
            }
        }
        None
    }

    fn match_memo(&self, other: &str) -> bool {
        self.memo.to_lowercase() == other.to_lowercase()
            || self.memo.to_lowercase() == f!("no {}", self.memo.to_lowercase())
    }

    fn decode_memo(&self) -> Result<String> {
        let decoded = bs58::decode(&self.memo).into_vec()?;
        let value = &decoded[3..decoded[2] as usize + 3];
        Ok(String::from_utf8(value.to_vec())?)
    }
}

impl W<Vec<MinaVote>> {
    pub(crate) fn process(self, key: impl Into<String>, tip: i64) -> Self {
        let mut map = HashMap::new();
        let key = key.into();

        for mut vote in self.0 {
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

        W(map.values().cloned().collect())
    }

    pub(crate) fn process_weighted(
        self,
        key: impl Into<String>,
        ledger: &Ledger,
        tip: i64,
    ) -> Self {
        let key = key.into();
        let votes = self.process(key, tip).0;

        let votes_with_stake: Vec<MinaVote> = votes
            .into_iter()
            .filter_map(|mut vote| {
                let stake_weight = get_stake_weight(ledger, &vote.account).ok()?;
                vote.update_stake(stake_weight);
                Some(vote)
            })
            .collect();

        W(votes_with_stake)
    }

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

        assert_eq!(v1_decoded, "no cftest-2");
        assert_eq!(v2_decoded, "no cftest-2");
        assert_eq!(v3_decoded, "no cftest-2");
        assert_eq!(v4_decoded, "cftest-2");
    }

    #[test]
    fn test_process_votes() {
        let votes = get_test_votes();
        let processed = W(votes).process("cftest-2", 129).0;

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
