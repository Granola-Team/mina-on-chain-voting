// @generated automatically by Diesel CLI.

pub mod sql_types {
    #[derive(diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "proposal_version"))]
    pub struct ProposalVersion;
}

diesel::table! {
    use diesel::sql_types::*;
    use super::sql_types::ProposalVersion;

    mina_proposals (id) {
        id -> Int4,
        key -> Text,
        start_time -> Int8,
        end_time -> Int8,
        ledger_hash -> Nullable<Text>,
        version -> ProposalVersion,
    }
}
