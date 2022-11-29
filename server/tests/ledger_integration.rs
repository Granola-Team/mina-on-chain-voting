use osc_api::{
    models::{BlockStatus, SignalTrainsaction, SignalStats, SignalStatus},
    processor::SignalProcessor,
    utils,
};

#[test]
pub fn undelegated_account_unsettled_signal_adds_balance_to_yes() {
    let mock = "undelegated_account";
    let key = "magenta";
    let signal = SignalTrainsaction {
        account: "A".to_string(),
        memo: utils::encode_transaction_memo("magenta"),
        height: 0,
        nonce: 0,
        status: osc_api::models::BlockStatus::Canonical,
        timestamp: 0,
    };
    utils::assert_signal_stats(
        mock,
        signal.clone(),
        key,
        0,
        SignalStats { yes: 1.0, no: 0.0 },
    );
    utils::assert_signal_status(mock, signal, key, 0, SignalStatus::Unsettled);
}

#[test]
pub fn delegated_account_unsettled_signal_adds_balance_to_yes() {
    let mock = "delegated_account";
    let key = "magenta";
    let signal = SignalTrainsaction {
        account: "A".to_string(),
        memo: utils::encode_transaction_memo("magenta"),
        height: 0,
        nonce: 0,
        status: osc_api::models::BlockStatus::Canonical,
        timestamp: 0,
    };
    utils::assert_signal_stats(
        "delegated_account",
        signal.clone(),
        "magenta",
        0,
        SignalStats { yes: 1.0, no: 0.0 },
    );
    utils::assert_signal_status(mock, signal, key, 0, SignalStatus::Unsettled);
}

#[test]
pub fn undelegated_account_unsettled_signal_adds_balance_to_no() {
    let mock = "undelegated_account";
    let key = "magenta";
    let signal = SignalTrainsaction {
        account: "A".to_string(),
        memo: utils::encode_transaction_memo("no magenta"),
        height: 0,
        nonce: 0,
        status: osc_api::models::BlockStatus::Canonical,
        timestamp: 0,
    };
    utils::assert_signal_stats(
        mock,
        signal.clone(),
        key,
        0,
        SignalStats { yes: 0.0, no: 1.0 },
    );
    utils::assert_signal_status(mock, signal, key, 0, SignalStatus::Unsettled);
}

#[test]
pub fn delegated_account_unsettled_signal_adds_balance_to_no() {
    let mock = "delegated_account";
    let key = "magenta";
    let signal = SignalTrainsaction {
        account: "A".to_string(),
        memo: utils::encode_transaction_memo("no magenta"),
        height: 0,
        nonce: 0,
        status: BlockStatus::Canonical,
        timestamp: 0,
    };
    utils::assert_signal_stats(
        mock,
        signal.clone(),
        key,
        0,
        SignalStats { yes: 0.0, no: 1.0 },
    );
    utils::assert_signal_status(mock, signal, key, 0, SignalStatus::Unsettled);
}

#[test]
pub fn undelegated_account_settled_signal_adds_balance_to_yes() {
    let mock = "undelegated_account";
    let key = "magenta";
    let signal = SignalTrainsaction {
        account: "A".to_string(),
        memo: utils::encode_transaction_memo("magenta"),
        height: 0,
        nonce: 0,
        status: BlockStatus::Canonical,
        timestamp: 0,
    };
    utils::assert_signal_stats(
        mock,
        signal.clone(),
        "magenta",
        20,
        SignalStats { yes: 1.0, no: 0.0 },
    );
    utils::assert_signal_status(mock, signal, key, 20, SignalStatus::Settled);
}

#[test]
pub fn delegated_account_settled_signal_adds_balance_to_yes() {
    let mock = "delegated_account";
    let key = "magenta";
    let signal = SignalTrainsaction {
        account: "A".to_string(),
        memo: utils::encode_transaction_memo("magenta"),
        height: 0,
        nonce: 0,
        status: BlockStatus::Canonical,
        timestamp: 0,
    };
    utils::assert_signal_stats(
        mock,
        signal.clone(),
        "magenta",
        20,
        SignalStats { yes: 1.0, no: 0.0 },
    );
    utils::assert_signal_status(mock, signal, key, 20, SignalStatus::Settled);
}

#[test]
pub fn undelegated_account_settled_signal_adds_balance_to_no() {
    let mock = "undelegated_account";
    let key = "magenta";
    let signal = SignalTrainsaction {
        account: "A".to_string(),
        memo: utils::encode_transaction_memo("no magenta"),
        height: 0,
        nonce: 0,
        status: BlockStatus::Canonical,
        timestamp: 0,
    };
    utils::assert_signal_stats(
        mock,
        signal.clone(),
        "magenta",
        20,
        SignalStats { yes: 0.0, no: 1.0 },
    );
    utils::assert_signal_status(mock, signal, key, 20, SignalStatus::Settled);
}

#[test]
pub fn delegated_account_settled_signal_adds_balance_to_no() {
    let mock = "delegated_account";
    let key = "magenta";
    let signal = SignalTrainsaction {
        account: "A".to_string(),
        memo: utils::encode_transaction_memo("no magenta"),
        height: 0,
        nonce: 0,
        status: BlockStatus::Canonical,
        timestamp: 0,
    };
    utils::assert_signal_stats(
        mock,
        signal.clone(),
        "magenta",
        20,
        SignalStats { yes: 0.0, no: 1.0 },
    );
    utils::assert_signal_status(mock, signal, key, 20, SignalStatus::Settled);
}

#[test]
pub fn delegated_account_invalid_signal() {
    let mock = "delegated_account";
    let key = "magenta";
    let signal = SignalTrainsaction {
        account: "A".to_string(),
        memo: utils::encode_transaction_memo("invalid magenta signal"),
        height: 0,
        nonce: 0,
        status: BlockStatus::Canonical,
        timestamp: 0,
    };

    utils::assert_signal_status(mock, signal, key, 0, SignalStatus::Invalid);
}

#[test]
pub fn undelegated_account_invalid_signal() {
    let mock = "undelegated_account";
    let key = "magenta";
    let signal = SignalTrainsaction {
        account: "A".to_string(),
        memo: utils::encode_transaction_memo("invalid magenta signal"),
        height: 0,
        nonce: 0,
        status: BlockStatus::Canonical,
        timestamp: 0,
    };

    utils::assert_signal_status(mock, signal, key, 0, SignalStatus::Invalid);
}

#[test]
pub fn repeat_signals_use_hash_map() {
    // this test mainly is used to verify that
    // signalling still works when using the
    // internal hashmap to store accounts
    // an account is added to this hashmap
    // after the first time a signal from it
    // is processed, thus only the second
    // signal from any said account will
    // query the internal hashmap caching system
    // instead of asking the ledger
    utils::with_ledger_mock("delegated_account", |ledger| {
        let mut conn = ledger.db;
        let key = "magenta";
        let signal = SignalTrainsaction {
            account: "A".to_string(),
            memo: utils::encode_transaction_memo("magenta"),
            height: 0,
            nonce: 0,
            status: BlockStatus::Canonical,
            timestamp: 0,
        };
        let signals = vec![signal.clone(), signal];
        let response_entity = SignalProcessor::new(&mut conn, &key, 0, signals).run();

        assert_eq!(
            Some(SignalStats { yes: 1.0, no: 0.0 }),
            response_entity.stats
        );
    });
}

#[test]
pub fn repeat_settled_signals_bypass_signal_status_check() {
    // this test ensures that
    // if a signal is processed from an
    // account that has already had a signal
    // processed, the signal is not again
    // reclassified as Settled or Unsettled
    utils::with_ledger_mock("delegated_account", |ledger| {
        let mut conn = ledger.db;
        let key = "magenta";
        let signal = SignalTrainsaction {
            account: "A".to_string(),
            memo: utils::encode_transaction_memo("magenta"),
            height: 0,
            nonce: 0,
            status: BlockStatus::Canonical,
            timestamp: 0,
        };
        let signals = vec![signal.clone(), signal];
        let response_entity = SignalProcessor::new(&mut conn, &key, 20, signals).run();

        assert!(response_entity.settled.len() > 0);
        assert_eq!(
            Some(SignalStats { yes: 1.0, no: 0.0 }),
            response_entity.stats
        );
    });
}
