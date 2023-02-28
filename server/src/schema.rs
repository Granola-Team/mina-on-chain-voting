// @generated automatically by Diesel CLI.

diesel::table! {
    mina_proposals (id) {
        id -> Int4,
        key -> Text,
        global_start_slot -> Int8,
        global_end_slot -> Int8,
        ledger_hash -> Nullable<Text>,
    }
}
