use bs58::decode::Error as BS58Error;
use reqwest::Error as ReqwestError;
use serde_json::Error as JsonError;
use std::string::FromUtf8Error;

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

    #[error("UTF8 error: {0}")]
    UTF8(#[from] FromUtf8Error),

    #[error("BS58 error: {0}")]
    Base58(#[from] BS58Error),
}
