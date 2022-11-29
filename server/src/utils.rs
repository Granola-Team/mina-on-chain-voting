use std::{panic, collections::VecDeque, env::VarError};

use crate::{
    ledger::{HasConnection, Ledger},
    models::{DBResponse, SignalStats, SignalStatus},
    processor::SignalProcessor, Config, SubCommand,
};
use axum::{body::Body, http::Request, Router};
use base58check::ToBase58Check;
use tower::ServiceExt;

pub async fn run_router_request(
    app: Router,
    url: &'static str,
    body: Body,
) -> axum::response::Response {
    app.oneshot(
        Request::builder()
            .uri(url)
            .body(body)
            .unwrap_or_else(|_| panic!("Could not create Request")),
    )
    .await
    .unwrap_or_else(|_| panic!("Could not create App"))
}

pub fn encode_transaction_memo(memo: &str) -> String {
    let bytes = memo.as_bytes();
    let mut encoded = Vec::new();
    encoded.push(1);
    encoded.push(memo.len() as u8);
    for byte in bytes.iter() {
        encoded.push(*byte);
    }

    encoded.as_slice().to_base58check(0)
}

pub fn with_ledger_mock<T, C>(mock: &str, test: T)
where
    Ledger<C>: HasConnection,
    T: FnOnce(Ledger<C>),
{
    let filename = format!("./tests/__mocks__/{}.json", mock);
    let ledger = HasConnection::init(&filename)
        .unwrap_or_else(|_| panic!("Could not init Ledger at {:?}", filename));
    test(ledger)
}

pub fn assert_signal_stats(
    mock: &str,
    signal: DBResponse,
    key: &str,
    latest_block: i64,
    signal_stats: SignalStats,
) {
    with_ledger_mock(mock, |ledger| {
        let mut conn = ledger.db;
        let signals = vec![signal];
        let response_entity = SignalProcessor::new(&mut conn, key, latest_block, signals).run();

        assert_eq!(Some(signal_stats), response_entity.stats);
    });
}

pub fn assert_signal_status(
    mock: &str,
    signal: DBResponse,
    key: &str,
    latest_block: i64,
    signal_status: SignalStatus,
) {
    with_ledger_mock(mock, |ledger| {
        let mut conn = ledger.db;
        let signals = vec![signal];
        let response_entity = SignalProcessor::new(&mut conn, key, latest_block, signals).run();

        match signal_status {
            SignalStatus::Settled => assert!(!response_entity.settled.is_empty()),
            SignalStatus::Unsettled => assert!(!response_entity.unsettled.is_empty()),
            SignalStatus::Invalid => assert!(!response_entity.invalid.is_empty()),
        }
    });
}

pub fn create_config() -> Option<Config> {
    dotenv::dotenv().ok();
    if let Ok(mut env_vars) = vec![
    "MAINNET_DATABASE_URL",
    "DEVNET_DATABASE_URL",
    "MAINNET_LEDGER_PATH",
    "DEVNET_LEDGER_PATH",
    ]
    .into_iter()
    .map(std::env::var)
    .collect::<Result<VecDeque<String>, VarError>>()
    {
        let config = Config {
            mainnet_database_url: env_vars
            .pop_front()
            .unwrap_or_else(|| panic!("Error: Missing MAINNET_DATABASE_URL")),
            devnet_database_url: env_vars
            .pop_front()
            .unwrap_or_else(|| panic!("Error: Missing DEVNET_DATABASE_URL")),
            client_path: "../client/build".to_string(),
            mainnet_ledger_path: env_vars
            .pop_front()
            .unwrap_or_else(|| panic!("Error: Missing MAINNET_LEDGER")),
            devnet_ledger_path: env_vars
            .pop_front()
            .unwrap_or_else(|| panic!("Error: Missing DEVNET_LEDGER")),
            subcmd: SubCommand::Start,
        };

        return Some(config);
    }
    println!("Unable to test api context, secrets missing!");
    None
}