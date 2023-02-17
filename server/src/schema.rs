// @generated automatically by Diesel CLI.

diesel::table! {
    mina_proposals (id) {
        id -> Uuid,
        key -> Text,
        global_start_slot -> Int4,
        global_end_slot -> Int4,
    }
}
