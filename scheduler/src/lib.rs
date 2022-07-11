use std::collections::HashMap;

pub mod handlers;

pub mod database;

pub mod error;

pub fn decode_memo(memo: &str) -> Option<String> { // possible change to &str to remove heap allocation
    use base58check::FromBase58Check;

    match memo.from_base58check() {
        Ok((_ver, bytes)) => {
            match std::str::from_utf8(&bytes) {
                Ok(str) => {
                    match str.contains("magenta") {
                        true => Some(str.to_string()), // heap allocation
                        false => None,
                    }
                },
                Err(_) => None
            }
        },
        Err(_) => None
    }
}

pub fn parse_query_response(query_responses: &[database::QueryResponse]) -> database::VotesMap {
    let mut votes_map: database::VotesMap = HashMap::new();

    query_responses
        .iter()
        .for_each(|database::QueryResponse {
            account,
            memo, 
            height
        }| {
            if let Some(memo_str) = crate::decode_memo(memo) {
                if let Some((_prev_memo, prev_height)) = votes_map.get(account) {
                    if prev_height > height {
                        return;
                    }
                }

                votes_map.insert(
                    account.clone(), 
                    (memo_str, *height))
                    .unwrap();
            }
        });
        

    votes_map
}

pub fn gen_output(votes_map: database::VotesMap) -> database::APIResponse {
    votes_map
        .iter()
        .map(|(acct, (memo, _))| {
            (acct.clone(), memo.clone())
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decode() {
        let  test: database::QueryResponse = database::QueryResponse { 
            account: String::from("B62qmwnAmn1ZErZmFkGbFh2CnjK1GCCKkxBuhFnmeV3MXbx88wdNnFg"),
            memo: String::from("E4YbUJfcgcWB7AmbAMnpYQcjGbPmdG3iWGExrjTC97q2sq4a1YrYN"),
            height: 89412
        };

        let res = decode_memo(&test.memo);
        assert_ne!(None, res);
    }
}