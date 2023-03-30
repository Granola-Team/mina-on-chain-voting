use diesel::Queryable;
use diesel_derive_enum::DbEnum;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, DbEnum)]
#[ExistingTypePath = "crate::schema::sql_types::ProposalVersion"]
pub(crate) enum ProposalVersion {
    V1,
    V2,
}

#[derive(Queryable, Serialize, Deserialize, Debug)]
pub(crate) struct MinaProposal {
    pub(crate) id: i32,
    pub(crate) key: String,
    pub(crate) start_time: i64,
    pub(crate) end_time: i64,
    pub(crate) ledger_hash: Option<String>,
    pub(crate) version: ProposalVersion,
}
