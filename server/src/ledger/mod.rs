use std::{io::BufReader, fs::File, path::Path};
use rusqlite::Connection;
use serde::{Deserialize, Serialize};
use log::info;

mod stream;

#[derive(Debug, Default, PartialEq, Eq, Clone, FromSql, Serialize, Deserialize)]
pub struct LedgerDelegations {
    pub delegated_balance: String,
    pub total_delegators: i32,
}

impl LedgerDelegations {
    fn is_default(&self) -> bool {
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
    pub reader: BufReader<File>,
    pub db: Connection,
}

impl Ledger {
    pub fn init() -> Self {
        let path = std::env::var("LEDGER_PATH").expect("Environment: LEDGER_PATH not found.");
        
        if Path::new("temp").exists() {
            std::fs::remove_dir_all("temp").expect("Error: Could not remove local db artifacts.")
        }

        std::fs::create_dir("temp").expect("Error: Could not create local directory.");

        let reader = BufReader::new(File::open(path).expect("Error: Could not open ledger."));
        let db = Connection::open("temp/mem.db").expect("Error: Could not open local database.");

        Self { reader, db }
    }

    pub fn connection() -> Connection {
        Connection::open("temp/mem.db").expect("Error: Could not open local database.")
    }

    pub fn migrate(self) -> anyhow::Result<()> {
        info!("Starting Ledger migration...");

        self.db.execute(
            "CREATE TABLE IF NOT EXISTS Ledger (
                 pk text primary key,
                 balance text not null,
                 delegate text not null
             )",
            [],
        ).expect("Error: Ledger table creation failed.");

        info!("Processing records...");

        let mut entry_count = 0;
        for (_, res) in stream::iter::<LedgerEntity, BufReader<File>>(self.reader) {
            match res {
             Ok(x) => {
                let op = self.db.execute("INSERT INTO Ledger (pk, balance, delegate) values (?1, ?2, ?3)", [x.pk, x.balance, x.delegate]);

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
        Ok(())
    }
}

pub trait Query {
    fn get_stake(self, address: &str) -> Option<LedgerDelegations>;
    fn get_total_ledger_stake(self) -> String;
}

impl Query for Connection {
    fn get_stake(self, address: &str) -> Option<LedgerDelegations>  {
        let mut stmt = self.prepare(
        "
                SELECT CAST(SUM(CAST(balance AS DECIMAL)) AS TEXT), COUNT(pk) as delegators
                FROM Ledger
                WHERE delegate = (?)
                GROUP BY delegate
            "
        ).expect("Error preparing statement.");

        let rows_iter = stmt.query_map([&address], |row| {
            Ok(LedgerDelegations {
                    delegated_balance: row.get(0).unwrap_or_default(),
                    total_delegators: row.get(1).unwrap_or_default(),
                })
        }).expect("Error: Error unwrapping rows.");

        let mut stake: LedgerDelegations = LedgerDelegations::default();

        for res in rows_iter {
            match res {
                Ok(x) => { stake = x },
                Err(err) => println!("{}", err)
            }
        }

        if stake.is_default() {
            return None
        }

        Some(stake)
    }

    fn get_total_ledger_stake(self) -> String {
        let mut stmt = self.prepare(
            "
                    SELECT CAST(SUM(CAST(balance AS DECIMAL)) AS TEXT)
                    FROM Ledger
                    LIMIT 1
                "
            ).expect("Error preparing statement.");
            
        let mut rows = stmt.query([]).expect("Error: Error unwrapping rows.");
        let mut results: Vec<String> = Vec::new();

        while let Some(row) = rows.next().expect("Error selecting next row."){
            results.push(row.get(0).expect("Error getting row."));
        }

        results.into_iter().collect::<String>()
    }
}

