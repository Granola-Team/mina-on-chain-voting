mod tests {
    use on_chain_signalling_api::decode_memo;

    #[test]
    fn test_decode() {
        let res = decode_memo("E4YbUJfcgcWB7AmbAMnpYQcjGbPmdG3iWGExrjTC97q2sq4a1YrYN");
        assert_ne!(None, res);
    }
}