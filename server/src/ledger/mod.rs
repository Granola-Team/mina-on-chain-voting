use log::{error, info};
use serde::{Deserialize, Serialize};
use std::{fs::File, io::BufReader};
use tokio_rusqlite::Connection;

mod stream;

#[derive(Debug, Default, PartialEq, Eq, Clone, FromSql, Serialize, Deserialize)]
pub struct LedgerDelegations {
    pub delegated_balance: String,
    pub total_delegators: i32,
}

impl LedgerDelegations {
    pub fn is_default(&self) -> bool {
        self.eq(&LedgerDelegations::default())
    }
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
pub struct LedgerPermissions {
    pub stake: bool,
    pub edit_state: String,
    pub send: String,
    pub set_delegate: String,
    pub set_permissions: String,
    pub set_verification_key: String,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
pub struct LedgerTiming {
    pub initial_minimum_balance: String,
    pub cliff_time: String,
    pub cliff_amount: String,
    pub vesting_period: String,
    pub vesting_increment: String,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
pub struct LedgerTokenPermissions {}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
pub struct LedgerEntity {
    pub pk: String,
    pub balance: String,
    pub delegate: String,
    pub timing: Option<LedgerTiming>,
    pub token: String,
    pub token_permissions: Option<LedgerTokenPermissions>,
    pub nonce: Option<String>,
    pub receipt_chain_hash: String,
    pub voting_for: String,
    pub permissions: Option<LedgerPermissions>,
}

pub struct Ledger<T> {
    pub db: T,
}

pub trait HasConnection
where
    Self: Sized,
{
    type Connection;
    fn connection() -> Self::Connection;
    fn init(path: &str) -> anyhow::Result<Self>;
}

#[async_trait::async_trait]
pub trait HasConnectionAsync
where
    Self: Sized,
{
    type AsyncConnection;
    async fn connection() -> Self::AsyncConnection;
    async fn init_async(path: &str) -> anyhow::Result<Self>;
}

impl HasConnection for Ledger<rusqlite::Connection> {
    type Connection = rusqlite::Connection;

    fn connection() -> Self::Connection {
        rusqlite::Connection::open_in_memory().expect("Error: Could not create database.")
    }

    fn init(path: &str) -> anyhow::Result<Self> {
        let reader = BufReader::new(File::open(path).expect("Error: Could not open ledger."));
        let db =
            rusqlite::Connection::open_in_memory().expect("Error: Could not create connection.");
        let ledger: Ledger<Self::Connection> = Ledger { db };

        let conn = &ledger.db;
        info!("Starting Ledger migration...");

        let res = conn.execute(
            "CREATE TABLE Ledger (
                pk text primary key,
                balance text not null,
                delegate text not null
            )",
            [],
        );

        if let Err(e) = res {
            error!("Error creating table: {}", e);
        }

        info!("Processing records...");

        let mut entry_count = 0;
        for (index, res) in stream::iter::<LedgerEntity, BufReader<File>>(reader) {
            info!("Processing record: {}", index);
            match res {
                Ok(x) => {
                    let op = conn.execute(
                        "INSERT INTO Ledger (pk, balance, delegate) values (?1, ?2, ?3)",
                        [x.pk, x.balance, x.delegate],
                    );

                    match op {
                        Ok(_) => {
                            entry_count += 1;
                        }
                        Err(err) => eprintln!("{}", err),
                    }
                }
                Err(err) => eprintln!("{}", err),
            }
        }

        info!("Ledger Migration has finished.");
        info!("Total records processed: {}", entry_count);

        Ok(ledger)
    }
}

#[async_trait::async_trait]
impl HasConnectionAsync for Ledger<tokio_rusqlite::Connection> {
    type AsyncConnection = tokio_rusqlite::Connection;

    async fn connection() -> Connection {
        Connection::open_in_memory()
            .await
            .expect("Error: Could not create database.")
    }

    async fn init_async(path: &str) -> anyhow::Result<Self> {
        let reader = BufReader::new(
            File::open(path).unwrap_or_else(|_| panic!("Error: Could not open ledger {}.", path)),
        );
        let db = Connection::open_in_memory()
            .await
            .expect("Error: Could not create connection.");
        let ledger = Ledger { db };

        info!("Starting Ledger migration...");

        ledger
            .db
            .call(|conn| {
                let res = conn.execute(
                    "CREATE TABLE Ledger (
                pk text primary key,
                balance text not null,
                delegate text not null
            )",
                    [],
                );

                if let Err(e) = res {
                    error!("Error creating table: {}", e);
                }

                info!("Processing records...");

                let mut entry_count = 0;
                for (index, res) in stream::iter::<LedgerEntity, BufReader<File>>(reader) {
                    info!("Processing record: {}", index);
                    match res {
                        Ok(x) => {
                            let op = conn.execute(
                                "INSERT INTO Ledger (pk, balance, delegate) values (?1, ?2, ?3)",
                                [x.pk, x.balance, x.delegate],
                            );

                            match op {
                                Ok(_) => {
                                    entry_count += 1;
                                }
                                Err(err) => eprintln!("{}", err),
                            }
                        }
                        Err(err) => eprintln!("{}", err),
                    }
                }

                info!("Ledger Migration has finished.");
                info!("Total records processed: {}", entry_count);
            })
            .await;

        Ok(ledger)
    }
}
