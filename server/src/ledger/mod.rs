use std::{io::BufReader, fs::File, path::Path};
use serde::{Deserialize, Serialize};
use tokio_rusqlite::Connection;
use log::{info, error};

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
    pub set_verification_key: String
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Debug)]
pub struct LedgerTiming {
    pub initial_minimum_balance: String,
    pub cliff_time: String,
    pub cliff_amount: String,
    pub vesting_period: String,
    pub vesting_increment: String
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
    pub permissions: Option<LedgerPermissions>
}

pub struct Ledger {
    pub db: Connection,
}

impl Ledger {
    pub async fn connection() -> Connection {
        Connection::open_in_memory().await.expect("Error: Could not create database.")
    }

    pub async fn init() -> anyhow::Result<Self> {
        let path = std::env::var("LEDGER_PATH").expect("Environment: LEDGER_PATH not found.");
        
        if Path::new("temp").exists() {
            std::fs::remove_dir_all("temp").expect("Error: Could not remove local db artifacts.")
        }

        std::fs::create_dir("temp").expect("Error: Could not create local directory.");

        let reader = BufReader::new(File::open(path).expect("Error: Could not open ledger."));
        let db = Connection::open_in_memory().await.expect("Error: Could not create connection.");
        let ledger = Ledger { db };

        info!("Starting Ledger migration...");

        ledger.db.call(|conn| {
        let res = conn.execute("CREATE TABLE Ledger (
                pk text primary key,
                balance text not null,
                delegate text not null
            )", []);
        
        if let Err(e) = res {
            error!("Error creating table: {}", e);
        }

        info!("Processing records...");

        let mut entry_count = 0;
        for (index, res) in stream::iter::<LedgerEntity, BufReader<File>>(reader) {
            info!("Processing record: {}", index);
            match res {
             Ok(x) => {
                let op = conn.execute("INSERT INTO Ledger (pk, balance, delegate) values (?1, ?2, ?3)", [x.pk, x.balance, x.delegate]);

                match op {
                    Ok(_) => { entry_count+=1; },
                    Err(err) => eprintln!("{}", err)
                }
             },
             Err(err) => eprintln!("{}", err)
            }
         }

        info!("Ledger Migration has finished.");
        info!("Total records processed: {}", entry_count);

        }).await;

        Ok(ledger)
    }
}
