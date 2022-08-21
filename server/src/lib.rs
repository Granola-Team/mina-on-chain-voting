pub mod constants;
pub mod db;
pub mod error;
pub mod models;
pub mod routes;
pub mod ledger;

use base58check::FromBase58Check;

#[macro_use]
extern crate postgres_derive;

pub fn decode_memo(memo: &str, keyword: &str) -> Option<String> {
    if let Ok((_ver, bytes)) = memo.from_base58check() {
        if *bytes.first()? != 1u8 {
            return None;
        };
        let end_idx = *bytes.get(1)? as usize + 2;
        match std::str::from_utf8(&bytes[2..end_idx]) {
            Ok(str) => match str.to_lowercase().contains(keyword) {
                true => Some(str.to_string()),
                false => None,
            },
            Err(_) => None,
        }
    } else {
        None
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_decode_memo() {
        let encoded: Vec<&str> = vec![
            "E4YbUJfcgcWB7AmbAMnpYQcjGbPmdG3iWGExrjTC97q2sq4a1YrYN",
            "E4YhK17G5ThFfQeyRsYZW6fnZuDL1muW1Zie851BhzPiZQSQbD8Km",
            "E4YrnAgSbN69gengqkgeGaC8NmXdGnoChqCaEh8pdLdmjAPmbAYGF",
            "E4Z8L86JBcuZ6R13Kadmbcof54pUJp7678yEehxnzSmAgDkP4igKj",
            "E4YjFkJuoWdTgXPGYgc6PSPntpjjWhL6pYgnP7EvkMHjdnsKSCXYX",
            "E4Yh4ujezyS8JHQ2Lhs4wZCNpF9ouSpEiFErdeBUx6XUdt2REvq9A",
            "E4Yj6UuKCNcwum3VoQjBX4rt2tDSS6WMJsPhs6gRQxHVHWWg4yQvz",
            "E4Ys7Z1Gg2wt4X19S7sv8K7eWLuB4RMB2SpqAcoJr51ihcqgqd5TS",
        ];

        let decoded: Vec<String> = vec![
            "magenta".to_string(),
            "no magenta".to_string(),
            "Magenta_magenta".to_string(),
            "Magenta Magenta Magenta".to_string(),
            "no_-magenta".to_string(),
            "No_magenta".to_string(),
            "Yes Magenta".to_string(),
            "yes for magenta".to_string(),
        ];

        let result: Vec<String> = encoded
            .iter()
            .map(|s| super::decode_memo(s, "magenta").unwrap())
            .collect();

        assert_eq!(result, decoded);
    }
}
