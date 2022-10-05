use osc_api::ledger::stream;
// #[cfg(tests)]
mod tests {
    #[test]
    fn reads_skipping_ws_reads() {
        let array_eights: [u8; 1] = [1];
        let result = osc_api::ledger::stream::read_skipping_ws(array_eights); 
    }
}