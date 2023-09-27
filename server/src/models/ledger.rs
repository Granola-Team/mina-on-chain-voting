use std::collections::HashMap;
use std::io::Read;

use anyhow::anyhow;
use anyhow::Context;
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};

use crate::config::NetworkConfig;
use crate::models::diesel::ProposalVersion;
use crate::models::vote::MinaVote;
use crate::prelude::*;

const LEDGER_BALANCE_SCALE: u32 = 9;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Ledger(pub(crate) Vec<LedgerAccount>);

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash)]
#[serde(rename_all = "camelCase")]
pub(crate) struct LedgerAccount {
    pub(crate) pk: String,
    pub(crate) balance: String,
    pub(crate) delegate: String,
}

impl Ledger {
    pub(crate) async fn fetch(
        hash: impl Into<String>,
        network: NetworkConfig,
        ledger_storage_location: &Option<String>,
    ) -> Result<Ledger> {
        let hash = hash.into();

        match ledger_storage_location {
            Some(ledger_storage_location) => {
                let mut bytes = Vec::new();

                std::fs::File::open(f!("{ledger_storage_location}/{hash}.json"))
                    .with_context(|| f!("failed to open ledger {hash}"))?
                    .read_to_end(&mut bytes)
                    .with_context(|| f!("failed to read ledger {hash}"))?;

                Ok(Ledger(serde_json::from_slice(&bytes).with_context(
                    || f!("failed to deserialize ledger {hash}"),
                )?))
            }
            None => {
                let ledger_url = format!(
                    "https://raw.githubusercontent.com/Granola-Team/mina-ledger/main/{network}/{hash}.json");

                let ledger_response = reqwest::get(&ledger_url)
                    .await
                    .with_context(|| format!("failed to fetch ledger from URL: {ledger_url}"))?;

                let ledger_bytes = ledger_response
                    .bytes()
                    .await
                    .with_context(|| "failed to parse ledger response body")?;

                Ok(Ledger(serde_json::from_slice(&ledger_bytes).with_context(
                    || format!("failed to deserialize ledger data from URL: {ledger_url}"),
                )?))
            }
        }
    }

    pub(crate) fn get_stake_weight(
        &self,
        map: &Wrapper<HashMap<String, MinaVote>>,
        version: &ProposalVersion,
        public_key: impl Into<String>,
    ) -> Result<Decimal> {
        let public_key = public_key.into();

        let account = self
            .0
            .iter()
            .find(|d| d.pk == public_key)
            .ok_or_else(|| anyhow!("account {public_key} not found in ledger"))?;

        let balance = account
            .balance
            .parse()
            .unwrap_or_else(|_| Decimal::new(0, LEDGER_BALANCE_SCALE));

        match version {
            ProposalVersion::V1 => {
                if account.delegate != public_key {
                    return Ok(Decimal::new(0, LEDGER_BALANCE_SCALE));
                }

                let delegators = self
                    .0
                    .iter()
                    .filter(|d| d.delegate == public_key && d.pk != public_key)
                    .collect::<Vec<&LedgerAccount>>();

                if delegators.is_empty() {
                    return Ok(balance);
                }

                let stake_weight =
                    delegators
                        .iter()
                        .fold(Decimal::new(0, LEDGER_BALANCE_SCALE), |acc, x| {
                            x.balance
                                .parse()
                                .unwrap_or_else(|_| Decimal::new(0, LEDGER_BALANCE_SCALE))
                                + acc
                        });

                Ok(stake_weight + balance)
            }
            ProposalVersion::V2 => {
                let delegators = self
                    .0
                    .iter()
                    .filter(|d| {
                        d.delegate == public_key && d.pk != public_key && !map.0.contains_key(&d.pk)
                    })
                    .collect::<Vec<&LedgerAccount>>();

                if delegators.is_empty() {
                    return Ok(balance);
                }

                let stake_weight =
                    delegators
                        .iter()
                        .fold(Decimal::new(0, LEDGER_BALANCE_SCALE), |acc, x| {
                            x.balance
                                .parse()
                                .unwrap_or_else(|_| Decimal::new(0, LEDGER_BALANCE_SCALE))
                                + acc
                        });

                Ok(stake_weight + balance)
            }
        }
    }

    #[allow(dead_code)] // will remove this method when called from the frontend
    pub(crate) fn calculate_total_stake(&self, version: &ProposalVersion, map: &Wrapper<HashMap<String, MinaVote>>) -> Decimal {
        let total_stake = self.0.iter().map(|account| {
            let is_delegate = account.delegate == account.pk;
            let is_in_voting_map = *version == ProposalVersion::V2 && map.0.contains_key(&account.pk);

            if is_delegate && (!is_in_voting_map || *version == ProposalVersion::V1) {
                account.balance
                    .parse::<Decimal>()
                    .unwrap_or_else(|_| Decimal::new(0, LEDGER_BALANCE_SCALE))
            } else {
                Decimal::new(0, LEDGER_BALANCE_SCALE)
            }
        }).sum();

        total_stake
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl LedgerAccount {
        pub fn new(pk: String, balance: String, delegate: String) -> LedgerAccount {
            LedgerAccount {
                pk,
                balance,
                delegate,
            }
        }
    }

    fn get_accounts() -> (
        LedgerAccount,
        LedgerAccount,
        LedgerAccount,
        LedgerAccount,
        LedgerAccount,
    ) {
        return (
            LedgerAccount::new("A".to_string(), "1".to_string(), "A".to_string()),
            LedgerAccount::new("B".to_string(), "1".to_string(), "B".to_string()),
            LedgerAccount::new("C".to_string(), "1".to_string(), "A".to_string()),
            LedgerAccount::new("D".to_string(), "1".to_string(), "A".to_string()),
            LedgerAccount::new("E".to_string(), "1".to_string(), "B".to_string()),
        );
    }

    fn get_votes() -> HashMap<String, MinaVote> {
        let mut map = HashMap::new();
        map.insert(
            "B".to_string(),
            MinaVote::new(
                "B".to_string(),
                "",
                "",
                1,
                crate::models::vote::MinaBlockStatus::Canonical,
                1,
                0,
            ),
        );

        map.insert(
            "C".to_string(),
            MinaVote::new(
                "C".to_string(),
                "",
                "",
                1,
                crate::models::vote::MinaBlockStatus::Canonical,
                1,
                0,
            ),
        );

        map
    }

    #[test]
    fn test_stake_weight_v1() {
        let (a, b, c, d, _) = get_accounts();
        let map = get_votes();

        // No account found - throw err.
        let error = Ledger::get_stake_weight(
            &Ledger(vec![a.clone(), b.clone(), c.clone(), d.clone()]),
            &Wrapper(map.clone()),
            &ProposalVersion::V1,
            "E",
        );
        assert_eq!(error.is_err(), true);

        // Delegated stake away - returns 0.000000000.
        let d_weight = Ledger::get_stake_weight(
            &Ledger(vec![a.clone(), b.clone(), c.clone(), d.clone()]),
            &Wrapper(map.clone()),
            &ProposalVersion::V1,
            "D",
        );
        assert_eq!(d_weight.unwrap(), Decimal::new(0, LEDGER_BALANCE_SCALE));

        // No delegators & delegated to self - returns balance.
        let b_weight = Ledger::get_stake_weight(
            &Ledger(vec![a.clone(), b.clone(), c.clone(), d.clone()]),
            &Wrapper(map.clone()),
            &ProposalVersion::V1,
            "B",
        );

        assert_eq!(
            b_weight.unwrap(),
            Decimal::new(1000000000, LEDGER_BALANCE_SCALE)
        );

        // Delegated to self & has delegators - returns balance + delegators.
        let a_weight = Ledger::get_stake_weight(
            &Ledger(vec![a.clone(), b.clone(), c.clone(), d.clone()]),
            &Wrapper(map.clone()),
            &ProposalVersion::V1,
            "A",
        );
        assert_eq!(
            a_weight.unwrap(),
            Decimal::new(3000000000, LEDGER_BALANCE_SCALE)
        );
    }

    #[test]
    fn test_stake_weight_v2() {
        let (a, b, c, d, e) = get_accounts();
        let map = get_votes();

        // No account found - throw err.
        let error = Ledger::get_stake_weight(
            &Ledger(vec![a.clone(), b.clone(), c.clone(), d.clone(), e.clone()]),
            &Wrapper(map.clone()),
            &ProposalVersion::V2,
            "F",
        );
        assert_eq!(error.is_err(), true);

        let a_weight = Ledger::get_stake_weight(
            &Ledger(vec![a.clone(), b.clone(), c.clone(), d.clone(), e.clone()]),
            &Wrapper(map.clone()),
            &ProposalVersion::V2,
            "A",
        );
        assert_eq!(
            a_weight.unwrap(),
            Decimal::new(2000000000, LEDGER_BALANCE_SCALE)
        );

        let b_weight = Ledger::get_stake_weight(
            &Ledger(vec![a.clone(), b.clone(), c.clone(), d.clone(), e.clone()]),
            &Wrapper(map.clone()),
            &ProposalVersion::V2,
            "B",
        );

        assert_eq!(
            b_weight.unwrap(),
            Decimal::new(2000000000, LEDGER_BALANCE_SCALE)
        );
    }
}
