use axum::http::StatusCode;
use axum::response::{IntoResponse, Response};

use bs58::decode::Error as BS58Error;
use diesel::result::Error as DieselError;
use r2d2::Error as R2D2Error;
use reqwest::Error as ReqwestError;
use serde_json::Error as JsonError;
use std::string::FromUtf8Error;

// TODO: rework error handling
#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Ledger error: {0}")]
    Ledger(String),

    #[error(transparent)]
    Diesel(#[from] DieselError),

    #[error(transparent)]
    R2D2(#[from] R2D2Error),

    #[error(transparent)]
    Serde(#[from] JsonError),

    #[error(transparent)]
    Reqwest(#[from] ReqwestError),

    #[error(transparent)]
    UTF8(#[from] FromUtf8Error),

    #[error(transparent)]
    Base58(#[from] BS58Error),
}

impl Error {
    fn status_code(&self) -> StatusCode {
        StatusCode::INTERNAL_SERVER_ERROR
    }
}

impl IntoResponse for Error {
    fn into_response(self) -> Response {
        (self.status_code(), "Internal Server Error").into_response()
    }
}
