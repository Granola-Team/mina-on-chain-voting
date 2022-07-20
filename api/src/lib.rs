pub mod handlers;
pub mod db;
pub mod error;

#[macro_use]
extern crate postgres_derive;

use base58check::FromBase58Check;
use db::queries::QueryResponse;

pub type VotesMap<'a> = std::collections::HashMap<String, db::queries::QueryResponse>;

pub fn decode_memo(memo: &str) -> Option<String> {
    match memo.from_base58check() {
        Ok((_ver, bytes)) => {
            if *bytes.first()? != 1u8 { return None };
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

pub fn remove_duplicates(query_responses: &[QueryResponse]) -> VotesMap {
    let mut hash: VotesMap = std::collections::HashMap::new();

    query_responses
        .iter()
        .for_each(|i| {
            if let Some(memo_str) = decode_memo(&i.memo) {
                match hash.get(&i.account) {
                    Some(_) => (),
                    None => {
                        hash.insert(
                            i.account.clone(), 
                            db::queries::QueryResponse {
                                account: i.account.clone(),
                                height: i.height,
                                memo: memo_str,
                                status: i.status.clone()
                            });
                    },
                }

            }            
        });
        
    hash        
}