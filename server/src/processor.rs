use std::collections::HashMap;

use base58check::FromBase58Check;

use crate::{
    constants::SETTLED_DENOMINATOR,
    ledger::LedgerDelegations,
    models::{BlockStatus, DBResponse, ResponseEntity, Signal, SignalStats, SignalStatus},
};

pub type AccountSignalsMap = HashMap<String, Vec<Signal>>;
pub type AccountSignalMap = HashMap<String, Signal>;

/// represents the state required to calculate on-chain signalling statistics
pub struct SignalProcessor<'a> {
    /// connection to the in-memory database used to store the Staking Ledger
    conn: &'a mut rusqlite::Connection,
    /// key to be used to validate signals as "key" or "no key"
    key: String,
    /// height of the most recent block since the last archive node query
    latest_block: i64,
    /// MINA transactions matching the Granola Archive Query to be processed for signals
    signal_transactions: Vec<DBResponse>,
    /// Cache of signals that have already been processed, indexed by the account that sent them
    signallers_cache: AccountSignalsMap,
    /// Cache of settled signals indexed by their account
    current_settled: AccountSignalMap,
    /// Cache of unsettled signals indexed by account
    current_unsettled: AccountSignalMap,
    /// Cache of signals that don't match the key schema, or have been shadowed by a more recent signal
    invalid_signals: Vec<Signal>,
}

impl<'a> SignalProcessor<'a> {
    /// Initializes a new SignalProcessor
    ///
    /// # Arguments
    ///
    /// * `conn` - A connection to an in-memory SQLite database representing the Current Staking Ledger
    /// * `key` - The signalling key used to validate signals as "key" or "no key"
    /// * `latest_block` - Height of the most recent MINA block to process transactions from
    /// * `signal_transactions` - MINA transactions matching the Granola Archive Query to be processed for signals
    pub fn new(
        conn: &'a mut rusqlite::Connection,
        key: &str,
        latest_block: i64,
        signal_transactions: Vec<DBResponse>,
    ) -> Self {
        SignalProcessor {
            conn,
            key: key.to_string(),
            latest_block,
            signal_transactions,
            signallers_cache: HashMap::new(),
            current_settled: HashMap::new(),
            current_unsettled: HashMap::new(),
            invalid_signals: Vec::new(),
        }
    }

    /// Gets the total MINA delegated to a specific account along with the total number of delegators
    ///
    /// # Arguments
    ///
    /// * `account` - The public key of the account to look up
    pub fn delegations(&mut self, account: &str) -> LedgerDelegations {
        let mut delegations: LedgerDelegations = LedgerDelegations::default();
        let mut stmt = self
            .conn
            .prepare(
                "
                SELECT
                CAST(SUM(CAST(balance AS DECIMAL)) AS TEXT),
                COUNT(pk) as delegators
                FROM Ledger
                WHERE delegate = (?)
                GROUP BY delegate
                ",
            )
            .expect("Error preparing statement.");

        for res in stmt
            .query_map([account.to_string()], |row| {
                Ok(LedgerDelegations {
                    delegated_balance: row.get(0).unwrap_or_default(),
                    total_delegators: row.get(1).unwrap_or_default(),
                })
            })
            .expect("Error: Error unwrapping rows.")
            .flatten()
        {
            delegations = res;
        }

        delegations
    }

    /// Decodes a string encoded with base58check and MINA metadata attached
    ///
    /// # Arguments
    ///
    /// * `encoded` - the encoded memo potentially decode
    pub fn decode_memo(&self, encoded: &str) -> Option<String> {
        if let Ok((_ver, bytes)) = encoded.from_base58check() {
            if *bytes.first()? != 1u8 {
                return None;
            };
            let end_idx = *bytes.get(1)? as usize + 2;
            match std::str::from_utf8(&bytes[2..end_idx]) {
                Ok(str) => match str.to_lowercase().contains(&self.key) {
                    true => Some(str.to_string()),
                    false => None,
                },
                Err(_) => None,
            }
        } else {
            None
        }
    }

    /// Pop a transaction from the queue, getting its account's Delegations and parsing its Signalling Status
    pub fn parse_next_transaction(&mut self) -> Option<Signal> {
        let transaction = self.signal_transactions.pop()?;
        let memo_decoded = self.decode_memo(&transaction.memo)?;

        let delegations = self.delegations(&transaction.account);
        if delegations.is_default() {
            return None;
        }

        let mut signal_status = SignalStatus::Invalid;
        if memo_decoded.to_lowercase() == self.key.to_lowercase()
            || memo_decoded.to_lowercase() == format!("no {}", self.key.to_lowercase())
        {
            if transaction.height + SETTLED_DENOMINATOR <= self.latest_block
                && matches!(transaction.status, BlockStatus::Canonical)
            {
                signal_status = SignalStatus::Settled;
            } else {
                signal_status = SignalStatus::Unsettled;
            }
        }

        let signal = Signal {
            account: transaction.account,
            memo: memo_decoded,
            height: transaction.height,
            status: transaction.status,
            timestamp: transaction.timestamp,
            nonce: transaction.nonce,
            delegations,
            signal_status,
        };
        Some(signal)
    }

    /// Potentially update a Signal-Account association with a new signal if the signal is newer
    ///
    /// # Arguments
    ///
    /// * `signals` - the Signal-Account association to update
    /// * `invalid_signals` - Bucket to dump whichever signal is lower
    /// * `signal` - the signal to potentially update with
    fn compare_current_assoc(
        signals: &mut AccountSignalMap,
        invalid_signals: &mut Vec<Signal>,
        mut signal: Signal,
    ) {
        match signals.get_mut(&signal.account) {
            Some(prev_signal) => {
                if is_higher(&signal, prev_signal) {
                    prev_signal.signal_status = SignalStatus::Invalid;
                    invalid_signals.push(prev_signal.clone());
                    *prev_signal = signal.clone();
                } else {
                    signal.signal_status = SignalStatus::Invalid;
                    invalid_signals.push(signal)
                }
            }
            None => {
                signals.insert(signal.account.clone(), signal);
            }
        }
    }

    /// Take a delegated signal and update state with classification info
    ///
    /// # Arguments
    ///
    /// * `signal` - the signal to process
    pub fn process_signal(&mut self, signal: Signal) {
        let signals = match self.signallers_cache.get_mut(&signal.account) {
            Some(signals) => signals,
            None => self
                .signallers_cache
                .entry(signal.account.clone())
                .or_insert_with_key(|_| Vec::new()),
        };
        signals.push(signal.clone());
        match signal.signal_status {
            SignalStatus::Settled => Self::compare_current_assoc(
                &mut self.current_settled,
                &mut self.invalid_signals,
                signal,
            ),
            SignalStatus::Unsettled => Self::compare_current_assoc(
                &mut self.current_unsettled,
                &mut self.invalid_signals,
                signal,
            ),
            SignalStatus::Invalid => self.invalid_signals.push(signal),
        }
    }

    /// Add a delegation to a draft signal total tabulation
    ///
    /// # Arguments
    ///
    /// * `stats` - the draft tabulation to update
    /// * `signal` - the signal to validate and tabulate
    pub fn add_delegation(&self, stats: &mut SignalStats, signal: &Signal) {
        let delegated_balance = signal
            .delegations
            .delegated_balance
            .parse::<f32>()
            .unwrap_or(0.00);

        if signal.memo.to_lowercase() == self.key.to_lowercase() {
            stats.yes += delegated_balance;
        }

        if signal.memo.to_lowercase() == format!("no {}", &self.key.to_lowercase()) {
            stats.no += delegated_balance;
        }
    }

    /// Calculate the scoring of the current signalling state, and return if non-zero
    pub fn stats(&self) -> Option<SignalStats> {
        let mut total_stats: SignalStats = Default::default();

        for (_account, signal) in self.current_settled.iter() {
            self.add_delegation(&mut total_stats, signal);
        }

        for (_account, signal) in self.current_unsettled.iter() {
            if self.current_settled.get(&signal.account).is_none() {
                self.add_delegation(&mut total_stats, signal);
            }
        }

        match total_stats.yes != 0. || total_stats.no != 0. {
            true => Some(total_stats),
            false => None,
        }
    }

    /// Create the data to be returned to the client
    fn generate_response(self) -> ResponseEntity {
        let stats = self.stats();
        let settled = self
            .current_settled
            .into_iter()
            .map(|(_, v)| v)
            .collect::<Vec<Signal>>();

        let unsettled = self
            .current_unsettled
            .into_iter()
            .map(|(_, v)| v)
            .collect::<Vec<Signal>>();

        ResponseEntity {
            settled,
            unsettled,
            invalid: self.invalid_signals,
            stats,
        }
    }

    /// Run this SignalProcessor as a state machine to generate a response
    pub fn run(mut self) -> ResponseEntity {
        while !self.signal_transactions.is_empty() {
            if let Some(signal) = self.parse_next_transaction() {
                self.process_signal(signal);
            }
        }

        self.generate_response()
    }
}

/// Compare if one signal is higher than another
fn is_higher(s1: &Signal, s2: &Signal) -> bool {
    s1.height > s2.height || (s1.height == s2.height && s1.nonce > s2.nonce)
}
