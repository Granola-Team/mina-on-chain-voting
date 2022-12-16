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

        return Ok(serde_json::from_slice(&ledger)?);
    }
}

pub fn get_stake_weight(
    ledger: &Vec<LedgerAccount>,
    public_key: impl Into<String>,
) -> anyhow::Result<f64> {
    let public_key = public_key.into();

    let _account = ledger.iter().find(|d| d.pk == public_key);
    let account = match _account {
        Some(account) => account,
        None => anyhow::bail!("Error: Account not found in ledger."),
    };

    let delegators = ledger
        .iter()
        .filter(|d| d.delegate == public_key)
        .collect::<Vec<&LedgerAccount>>();

    // PK is delegated to someone else.
    if account.delegate != public_key {
        return Ok(0.00);
    }

    // PK is delegated to itself.
    let stake_weight = delegators
        .iter()
        .fold(account.balance.parse::<f64>().unwrap_or(0.00), |acc, x| {
            x.balance.parse::<f64>().unwrap() + acc
        });

    Ok(stake_weight)
}
