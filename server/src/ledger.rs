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

pub fn get_staking_data(
    ledger: &Vec<LedgerAccount>,
    public_key: impl Into<String>,
) -> anyhow::Result<crate::signal::Stake> {
    let public_key = public_key.into();

    let _account = ledger.iter().find(|d| d.pk == public_key);
    let account = match _account {
        Some(account) => account,
        None => anyhow::bail!("Error: Account not found in ledger."),
    };

    let delegators_for_pk = ledger
        .iter()
        .filter(|d| d.delegate == public_key)
        .collect::<Vec<&LedgerAccount>>();

    let value = crate::signal::Stake {
        delegated_balance: delegators_for_pk
            .iter()
            .fold(0.00, |acc, x| x.balance.parse::<f64>().unwrap() + acc),
        total_delegators: delegators_for_pk.len() as i32,
    };

    Ok(value)
}

// let ledger = ledger::get_ledger(
//     "jxYFH645cwMMMDmDe7KnvTuKJ5Ev8zZbWtA73fDFn7Jyh8p6SwH",
//     &ctx.cache,
// )
// .await
// .unwrap();

// let delegations = ledger::get_staking_data(
//     &ledger,
//     "B62qmjZSQHakvWz7ZMkaaVW7ye1BpxdYABAMoiGk3u9bBaLmK5DJPkR",
// )
// .unwrap();
