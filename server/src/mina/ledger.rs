use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

use crate::db::cache::CacheManager;
use crate::prelude::*;

const LEDGER_BALANCE_SCALE: u32 = 9;

#[typeshare]
pub(crate) type Ledger = Vec<LedgerAccount>;

#[typeshare]
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash)]
#[serde(rename_all = "camelCase")]
pub(crate) struct LedgerAccount {
    pub pk: String,
    pub balance: String,
    pub delegate: String,
}

pub(crate) async fn get_ledger(hash: impl Into<String>, cache: &CacheManager) -> Result<Ledger> {
    let hash = hash.into();

    if let Some(cached) = cache.ledger.get(&hash) {
        match serde_json::from_slice::<Ledger>(&cached) {
            Ok(values) => Ok(values),
            Err(_) => Err(Error::Ledger(f!("parsing ledger '{hash}' failed."))),
        }
    } else {
        let ledger = reqwest::get(f!(
            "https://raw.githubusercontent.com/Granola-Team/mina-ledger/main/mainnet/{hash}.json"
        ))
        .await?
        .bytes()
        .await?;

        cache.ledger.insert(hash, Arc::new(ledger.clone())).await;

        Ok(serde_json::from_slice(&ledger)?)
    }
}

pub(crate) fn get_stake_weight(
    ledger: &[LedgerAccount],
    public_key: impl Into<String>,
) -> Result<Decimal> {
    let public_key = public_key.into();

    let account = ledger
        .iter()
        .find(|d| d.pk == public_key)
        .ok_or_else(|| Error::Ledger(f!("account '{public_key}' not found in ledger.")))?;

    let balance = account
        .balance
        .parse::<Decimal>()
        .unwrap_or_else(|_| Decimal::new(0, LEDGER_BALANCE_SCALE));

    if account.delegate != public_key {
        return Ok(Decimal::new(0, LEDGER_BALANCE_SCALE));
    }

    let delegators = ledger
        .iter()
        .filter(|d| d.delegate == public_key && d.pk != public_key)
        .collect::<Vec<&LedgerAccount>>();

    if delegators.is_empty() {
        return Ok(balance);
    }

    let stake_weight = delegators
        .iter()
        .fold(Decimal::new(0, LEDGER_BALANCE_SCALE), |acc, x| {
            x.balance
                .parse::<Decimal>()
                .unwrap_or_else(|_| Decimal::new(0, LEDGER_BALANCE_SCALE))
                + acc
        });

    Ok(stake_weight + balance)
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
        let error = get_stake_weight(&[a.clone(), b.clone(), c.clone(), d.clone()], "E");
        assert_eq!(error.is_err(), true);

        // Delegated stake away - returns 0.000000000.
        let d_weight =
            get_stake_weight(&[a.clone(), b.clone(), c.clone(), d.clone()], "D").unwrap();
        assert_eq!(d_weight, Decimal::new(0, LEDGER_BALANCE_SCALE));

        // No delegators & delegated to self - returns balance.
        let b_weight =
            get_stake_weight(&[a.clone(), b.clone(), c.clone(), d.clone()], "B").unwrap();
        assert_eq!(b_weight, Decimal::new(1000000000, LEDGER_BALANCE_SCALE));

        // Delegated to self & has delegators - returns balance + delegators.
        let a_weight = get_stake_weight(&[a, b, c, d], "A").unwrap();
        assert_eq!(a_weight, Decimal::new(3000000000, LEDGER_BALANCE_SCALE));
    }
}
