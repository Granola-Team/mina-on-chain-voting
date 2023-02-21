// @generated automatically by Diesel CLI.

diesel::table! {
    mina_proposals (id) {
        id -> Int4,
        key -> Text,
        global_start_slot -> Int4,
        global_end_slot -> Int4,
        ledger_hash -> Nullable<Text>,
    }
}
