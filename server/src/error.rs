use reqwest::Error as ReqwestError;
use serde_json::Error as JsonError;
use sqlx::Error as SqlxError;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Ledger error: {0}")]
    Ledger(String),

    #[error("Serde error: {0}")]
    Serde(#[from] JsonError),

    #[error("Reqwest error: {0}")]
    Reqwest(#[from] ReqwestError),

    #[error("SQLX error: {0}")]
    Sqlx(#[from] SqlxError),
}
