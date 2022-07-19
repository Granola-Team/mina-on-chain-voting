pub mod handlers;
pub mod db;
pub mod error;

pub type VotesMap = std::collections::HashMap<String, (String, i64)>;
pub type APIResponse = Vec<(String, String)>;

pub fn decode_memo(memo: &str) -> Option<String> { // possible change to &str to remove heap allocation
    match base58check::FromBase58Check::from_base58check(memo) {
        Ok((_ver, bytes)) => {

            if *bytes.first()? != 1u8 { return None };

            let end_idx = *bytes.first()? as usize + 2;

            match std::str::from_utf8(&bytes[2..end_idx]) {
                Ok(str) => {
                    match str.to_lowercase().contains("magenta") {
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


pub fn parse_query_response(query_responses: &[db::queries::QueryResponse]) -> VotesMap {
    let mut votes_map: VotesMap = std::collections::HashMap::new();

    query_responses
        .iter()
        .for_each(|db::queries::QueryResponse {
            account,
            memo, 
            height,
            // status
        }| {
            if let Some(memo_str) = crate::decode_memo(memo) {
                if let Some((_prev_memo, prev_height)) = votes_map.get(account) {
                    if prev_height < height {
                        votes_map.insert(
                            account.clone(), 
                            (memo_str, *height));
                    }
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