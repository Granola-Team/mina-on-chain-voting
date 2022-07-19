pub mod handlers;
pub mod db;
pub mod error;

#[macro_use]
extern crate postgres_derive;

use base58check::FromBase58Check;

pub type VotesMap = std::collections::HashMap<String, (String, i64)>;
pub type APIResponse = Vec<(String, String)>;

pub fn decode_memo(memo: &str) -> Option<String> {

    match memo.from_base58check() {
        Ok((_ver, bytes)) => {
            if *bytes.get(0)? != 1u8 { return None };
            let end_idx = *bytes.get(1)? as usize + 2;
            match std::str::from_utf8(&bytes[2..end_idx]) {
                Ok(str) => {
                    match str.to_lowercase().contains("magenta") {
                        true => Some(str.to_string()),
                        false => None,
                    }
                },
                Err(_) => None
            }
        },
        Err(_) => None
    }
}

pub fn parse_query_response(query_responses: &[db::queries::QueryResponse]) -> VotesMap {
    let mut votes_map: VotesMap = std::collections::HashMap::new();

    query_responses
        .iter()
        .for_each(|db::queries::QueryResponse {
            account,
            memo, 
            height,
            status
        }| {
            if let Some(memo_str) = decode_memo(memo) {
                match votes_map.get(account) {
                    Some((_prev_memo, prev_height)) => {
                        if prev_height > height {
                            return;
                        }
                    },
                    None => {
                        votes_map.insert(
                            account.clone(), 
                            (memo_str, *height));
                    },
                }

            }            
        });
        

    votes_map
}

pub fn gen_output(votes_map: VotesMap) -> APIResponse {
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
        let res = decode_memo("E4YbUJfcgcWB7AmbAMnpYQcjGbPmdG3iWGExrjTC97q2sq4a1YrYN");
        assert_ne!(None, res);
    }
}