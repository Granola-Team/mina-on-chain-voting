use std::collections::HashMap;

const QUERY_STATEMENT:
    &'static str = "
        SELECT pk.value as account, uc.memo as memo, height
        FROM user_commands AS uc
        JOIN blocks_user_commands AS buc
        ON uc.id = buc.user_command_id
        JOIN blocks AS b
        ON buc.block_id = b.id
        JOIN public_keys AS pk
        ON uc.source_id = pk.id
        WHERE uc.type = 'payment'
        AND uc.source_id = uc.receiver_id
        AND uc.token = 1
        AND b.chain_status = 'canonical'
        AND buc.status = 'applied'
        ;
    ";
    
pub struct QueryResponse {
    account: Vec<u8>,
    memo: String,
    height: u32
}
pub type VotesMap = std::collections::HashMap<Vec<u8>, (String, u32)>;
pub type APIResponse = Vec<(String, String)>;

pub fn decode_memo(memo: &str) -> Option<String> {
    use base58check::FromBase58Check;

    match memo.from_base58check() {
        Ok((_ver, bytes)) => {
            match std::str::from_utf8(&bytes) {
                Ok(str) => {
                    let memo_string = String::from(str);
                    match memo_string.contains("magenta") {
                        true => Some(memo_string),
                        false => None,
                    }
                },
                Err(_) => None
            }
        },
        Err(_) => None
    }
}

pub async fn query_database(
    pg_client: &tokio_postgres::Client
) -> Result<Vec<QueryResponse>, tokio_postgres::Error> {
    Ok(
        pg_client
            .query(QUERY_STATEMENT, &[])
            .await?.iter()
            .map(|row| {
                let account = row.get::<&str, Vec<u8>>("account");
                let memo = row.get::<&str, String>("memo");
                let height = row.get::<&str, u32>("height");

                QueryResponse { account, memo, height }
            })
            .collect()
    )
    
}

pub fn parse_query_response(query_responses: &[QueryResponse]) -> VotesMap {
    let mut votes_map: VotesMap = HashMap::new();

    query_responses
        .iter()
        .for_each(|QueryResponse {
            account,
            memo, 
            height
        }| {
            if let Some((_prev_memo, prev_height)) = votes_map.get(account) {
                if prev_height < height {
                    if let Some(memo_str) = decode_memo(memo) {
                        votes_map.insert(
                            account.clone(), 
                            (memo_str, *height))
                            .unwrap();
                    }
                }
            }
        });
        

    votes_map
}

pub fn gen_output(votes_map: VotesMap) -> APIResponse {
    votes_map
        .iter()
        .map(|(acct_bytes, (memo, _))| {
            let acct = String::from_utf8(acct_bytes.clone()).unwrap();
            (acct, memo.clone())
        })
        .collect()
}