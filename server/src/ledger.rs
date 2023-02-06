use moka::future::Cache;
use rust_decimal::Decimal;
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
) -> anyhow::Result<Decimal> {
    let public_key = public_key.into();

    let _account = ledger.iter().find(|d| d.pk == public_key);
    let account = match _account {
        Some(account) => account,
        None => anyhow::bail!("Error: Account not found in ledger."),
    };

    let balance = account.balance.parse::<Decimal>().unwrap_or(Decimal::new(0, 9));

    if account.delegate != public_key {
        return Ok(Decimal::new(0, 9));
    }

    let delegators = ledger
        .iter()
        .filter(|d| d.delegate == public_key && d.pk != public_key)
        .collect::<Vec<&LedgerAccount>>();

    if delegators.is_empty() {
        return Ok(balance);
    }

    let stake_weight = delegators.iter().fold(Decimal::new(0, 9), |acc, x| {
        x.balance.parse::<Decimal>().unwrap_or(Decimal::new(0, 9)) + acc
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

        // Delegated stake away - returns 0.000000000.
        let d_weight =
            get_stake_weight(&[a.clone(), b.clone(), c.clone(), d.clone()], "D").unwrap();
        console.log(print!("{}", d_weight));
        assert_eq!(d_weight, Decimal::new(0, 9));


        // No delegators & delegated to self - returns balance.
        let b_weight =
            get_stake_weight(&[a.clone(), b.clone(), c.clone(), d.clone()], "B").unwrap();
        print!("{}", b_weight);
        assert_eq!(b_weight, Decimal::new(1, 9));

        // Delegated to self & has delegators - returns balance + delegators.
        let a_weight = get_stake_weight(&[a, b, c, d], "A").unwrap();
        print!("{}", a_weight);
        assert_eq!(a_weight, Decimal::new(3, 9));
    }
/*
    use serde_json::{from_str, to_string};

    #[test]
    fn test_json_serialization() {
        let my_struct = LedgerAccount {
            field1: "value1",
            field2: 123,
        };

        let serialized = to_string(&my_struct).unwrap();
        let deserialized: MyStruct = from_str(&serialized).unwrap();

        assert_eq!(my_struct, deserialized);
    }

    #[test]
    fn test_json_type() {
        let my_struct = MyStruct {
            field1: "value1",
            field2: 123,
        };

        let serialized = to_string(&my_struct).unwrap();
        let deserialized: serde_json::Value = from_str(&serialized).unwrap();

        // Check if field1 is a string
        assert!(deserialized["field1"].is_string());

        // Check if field2 is a number
        assert!(deserialized["field2"].is_number());
    }
     */
}
