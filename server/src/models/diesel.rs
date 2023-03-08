use diesel::Queryable;
use serde::{Deserialize, Serialize};

#[derive(Queryable, Serialize, Deserialize, Debug)]
pub(crate) struct MinaProposal {
    pub(crate) id: i32,
    pub(crate) key: String,
    pub(crate) start_time: i64,
    pub(crate) end_time: i64,
    pub(crate) ledger_hash: Option<String>,
}
