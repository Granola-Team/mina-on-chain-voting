use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};

use crate::{prelude::*, config::NetworkConfig};

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
    pub(crate) async fn fetch(hash: impl Into<String>, network: NetworkConfig) -> Result<Ledger> {
        let hash = hash.into();

        let ledger = reqwest::get(f!(
            "https://raw.githubusercontent.com/Granola-Team/mina-ledger/main/{network}/{hash}.json"
        ))
        .await?
        .bytes()
        .await?;

        Ok(Ledger(serde_json::from_slice(&ledger)?))
    }

    pub(crate) fn get_stake_weight(&self, public_key: impl Into<String>) -> Result<Decimal> {
        let public_key = public_key.into();

        let account = self
            .0
            .iter()
            .find(|d| d.pk == public_key)
            .ok_or_else(|| Error::Ledger(f!("account '{public_key}' not found in ledger.")))?;

        let balance = account
            .balance
            .parse()
            .unwrap_or_else(|_| Decimal::new(0, LEDGER_BALANCE_SCALE));

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

    fn get_accounts() -> (LedgerAccount, LedgerAccount, LedgerAccount, LedgerAccount) {
        return (
            LedgerAccount::new("A".to_string(), "1".to_string(), "A".to_string()),
            LedgerAccount::new("B".to_string(), "1".to_string(), "B".to_string()),
            LedgerAccount::new("C".to_string(), "1".to_string(), "A".to_string()),
            LedgerAccount::new("D".to_string(), "1".to_string(), "A".to_string()),
        );
    }

    #[test]
    fn test_stake_weight() {
        let (a, b, c, d) = get_accounts();

        // No account found - throw err.
        let error = Ledger::get_stake_weight(
            &Ledger(vec![a.clone(), b.clone(), c.clone(), d.clone()]),
            "E",
        );
        assert_eq!(error.is_err(), true);

        // Delegated stake away - returns 0.000000000.
        let d_weight = Ledger::get_stake_weight(
            &Ledger(vec![a.clone(), b.clone(), c.clone(), d.clone()]),
            "D",
        );
        assert_eq!(d_weight.unwrap(), Decimal::new(0, LEDGER_BALANCE_SCALE));

        // No delegators & delegated to self - returns balance.
        let b_weight = Ledger::get_stake_weight(
            &Ledger(vec![a.clone(), b.clone(), c.clone(), d.clone()]),
            "B",
        );

        assert_eq!(
            b_weight.unwrap(),
            Decimal::new(1000000000, LEDGER_BALANCE_SCALE)
        );

        // Delegated to self & has delegators - returns balance + delegators.
        let a_weight = Ledger::get_stake_weight(
            &Ledger(vec![a.clone(), b.clone(), c.clone(), d.clone()]),
            "A",
        );
        assert_eq!(
            a_weight.unwrap(),
            Decimal::new(3000000000, LEDGER_BALANCE_SCALE)
        );
    }
}
