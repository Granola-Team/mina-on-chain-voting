use moka::future::Cache;
use serde::{Deserialize, Serialize};

pub type LedgerCache = Cache<String, std::sync::Arc<bytes::Bytes>>;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash)]
#[serde(rename_all = "camelCase")]
pub struct LedgerAccount {
    pub pk: String,
    pub balance: String,
    pub delegate: String,
}

impl LedgerAccount {
    pub fn new(pk: String, balance: String, delegate: String) -> Self {
        Self {
            pk,
            balance,
            delegate,
        }
    }
}

#[allow(clippy::string_add)]
pub async fn get_ledger(
    hash: impl Into<String>,
    cache: &LedgerCache,
) -> anyhow::Result<Vec<LedgerAccount>> {
    let hash = hash.into();

    if let Some(cached) = cache.get(&hash) {
        Ok(serde_json::from_slice::<Vec<LedgerAccount>>(&cached)?)
    } else {
        let ledger: bytes::Bytes = reqwest::get(format!(
            "https://raw.githubusercontent.com/Granola-Team/mina-ledger/main/mainnet/{}.json",
            hash
        ))
        .await?
        .bytes()
        .await?;

        cache
            .insert(hash, std::sync::Arc::new(ledger.clone()))
            .await;

        Ok(serde_json::from_slice(&ledger)?)
    }
}

pub fn get_stake_weight(
    ledger: &[LedgerAccount],
    public_key: impl Into<String>,
) -> anyhow::Result<f64> {
    let public_key = public_key.into();

    let _account = ledger.iter().find(|d| d.pk == public_key);
    let account = match _account {
        Some(account) => account,
        None => anyhow::bail!("Error: Account not found in ledger."),
    };

    let balance = account.balance.parse::<f64>().unwrap_or(0.00);

    if account.delegate != public_key {
        return Ok(0.00);
    }

    let delegators = ledger
        .iter()
        .filter(|d| d.delegate == public_key && d.pk != public_key)
        .collect::<Vec<&LedgerAccount>>();

    if delegators.is_empty() {
        return Ok(balance);
    }

    let stake_weight = delegators.iter().fold(0.00, |acc, x| {
        x.balance.parse::<f64>().unwrap_or(0.00) + acc
    });

    Ok(stake_weight + balance)
}

#[cfg(test)]
mod tests {
    use super::*;

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

        // Delegated stake away - returns 0.00.
        let d_weight =
            get_stake_weight(&[a.clone(), b.clone(), c.clone(), d.clone()], "D").unwrap();
        assert_eq!(d_weight, 0.00);

        // No delegators & delegated to self - returns balance.
        let b_weight =
            get_stake_weight(&[a.clone(), b.clone(), c.clone(), d.clone()], "B").unwrap();
        assert_eq!(b_weight, 1.00);

        // Delegated to self & has delegators - returns balance + delegators.
        let a_weight = get_stake_weight(&[a, b, c, d], "A").unwrap();
        assert_eq!(a_weight, 3.00);
    }
}
