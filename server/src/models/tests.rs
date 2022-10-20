/*******DBResponse Tests******
*****************************/

use super::*;
    use crate::ledger::LedgerDelegations;

    #[test]
    fn sort_ResponseEntity_sorts_signals_by_height() {
        let signal1 = Signal {
            account: String::from(""),
            memo: String::from(""),
            height: 8,
            status: BlockStatus::Canonical,
            timestamp: 10,
            delegations: Some(LedgerDelegations::default()),
            signal_status: Some(SignalStatus::Settled),
        };

        let signal2 = Signal {
            account: String::from(""),
            memo: String::from(""),
            height: 10,
            status: BlockStatus::Canonical,
            timestamp: 11,
            delegations: Some(LedgerDelegations::default()),
            signal_status: Some(SignalStatus::Settled),        
        };

        let response_entity = ResponseEntity {
            signals: vec! [signal1, signal2.clone()],
            stats: None,
        };
        let result = response_entity.sort(); 
        assert_eq!(result.signals.get(0).unwrap(), &signal2); 
    }

    #[test]
    fn stats_ResponseEntity_gives_signals_stats() {
        
    }