use std::panic;

use crate::{
    ledger::{HasConnection, Ledger},
    models::{DBResponse, SignalStats, SignalStatus},
    processor::SignalProcessor,
};
use axum::{body::Body, http::Request, Router};
use base58check::ToBase58Check;
use tower::ServiceExt;

pub async fn run_router_request(
    app: Router,
    url: &'static str,
    body: Body,
) -> axum::response::Response {
    app.oneshot(Request::builder().uri(url).body(body).unwrap())
        .await
        .unwrap()
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

pub fn with_ledger_mock<T, C>(mock: &str, test: T) -> ()
where
    Ledger<C>: HasConnection,
    T: FnOnce(Ledger<C>) -> () + panic::UnwindSafe,
{
    let filename = format!("./tests/__mocks__/{}.json", mock);
    let ledger = HasConnection::init(&filename).unwrap();
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
        let response_entity = SignalProcessor::new(&mut conn, &key, latest_block, signals).run();

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
        let response_entity = SignalProcessor::new(&mut conn, &key, latest_block, signals).run();

        match signal_status {
            SignalStatus::Settled => assert!(response_entity.settled.len() > 0),
            SignalStatus::Unsettled => assert!(response_entity.unsettled.len() > 0),
            SignalStatus::Invalid => assert!(response_entity.invalid.len() > 0),
        }
    });
}
