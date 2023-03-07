// @generated automatically by Diesel CLI.

diesel::table! {
    mina_proposals (id) {
        id -> Int4,
        key -> Text,
        start_time -> Int8,
        end_time -> Int8,
        ledger_hash -> Nullable<Text>,
    }
}
