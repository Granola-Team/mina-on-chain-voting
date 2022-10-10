// #[cfg(tests)]
mod tests {
    
    use osc_api::ledger::stream;

    #[test]
    fn read_skipping_ws_reads_data() {
        let array_eights: [u8; 1] = [1];
        let result = stream::read_skipping_ws(&array_eights[..]); 
        assert!(matches!(result, Result::Ok(_)));
        let result = result.unwrap();
        assert_eq!(result, 1);
    }

    #[test]
    fn read_skipping_ws_skips_whitespace() {
        let array_eights: [u8; 1] = [0x20];
        let result = stream::read_skipping_ws(&array_eights[..]);
        assert!(matches!(result, Result::Err(_))); 
    }
}