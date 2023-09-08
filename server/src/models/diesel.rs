use diesel::Queryable;
use diesel_derive_enum::DbEnum;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, DbEnum)]
#[ExistingTypePath = "crate::schema::sql_types::ProposalVersion"]
#[DbValueStyle = "SCREAMING_SNAKE_CASE"]
pub(crate) enum ProposalVersion {
    V1,
    V2,
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Serialize, Deserialize, Debug, DbEnum)]
#[ExistingTypePath = "crate::schema::sql_types::ProposalCategory"]
#[DbValueStyle = "verbatim"]
pub(crate) enum ProposalCategory {
    Core,
    Networking,
    Interface,
    ERC,
    Cryptography,
}

#[derive(Queryable, Serialize, Deserialize, Debug)]
pub(crate) struct MinaProposal {
    pub(crate) id: i32,
    pub(crate) key: String,
    pub(crate) start_time: i64,
    pub(crate) end_time: i64,
    pub(crate) ledger_hash: Option<String>,
    pub(crate) category: ProposalCategory,
    pub(crate) version: ProposalVersion,
    pub(crate) title: String,
    pub(crate) description: String,
    pub(crate) url: String,
}
