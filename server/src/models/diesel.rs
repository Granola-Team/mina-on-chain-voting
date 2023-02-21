use diesel::Queryable;
use serde::{Deserialize, Serialize};

#[derive(Queryable, Serialize, Deserialize, Debug)]
pub(crate) struct MinaProposal {
    pub(crate) id: i32,
    pub(crate) key: String,
    pub(crate) global_start_slot: i32,
    pub(crate) global_end_slot: i32,
    pub(crate) ledger_hash: Option<String>,
}
