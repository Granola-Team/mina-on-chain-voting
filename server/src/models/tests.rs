use super::*;

#[test]
fn response_entity_sort_sorts_signals_by_height() {
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
    fn response_entity_with_stats_adds_stats() {
        let response_entity = ResponseEntity::default();
        let stats = SignalStats { yes: 100., no: 50. };
        let with_stats = response_entity.with_stats(stats);
        assert_eq!(Some(stats), with_stats.stats);
    }