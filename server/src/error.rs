use reqwest::Error as ReqwestError;
use serde_json::Error as JsonError;

// TODO: Create proper error structure/messages
#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Ledger error: {0}")]
    Ledger(String),

    #[error("DB error: {0}")]
    Database(String),

    #[error("Serde error: {0}")]
    Serde(#[from] JsonError),

    #[error("Reqwest error: {0}")]
    Reqwest(#[from] ReqwestError),
}
