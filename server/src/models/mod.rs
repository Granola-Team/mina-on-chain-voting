use serde::{Serialize, Deserialize};

#[derive(Debug, PartialEq, Eq, FromSql, Serialize, Deserialize)]
pub struct DBResponse {  
        pub account: String,
        pub memo: String,
        pub height: i64,
        pub status: BlockStatus,
        pub signal_status: Option<Status>,
        pub timestamp: i64
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, FromSql, Serialize, Deserialize)]
#[postgres(name = "chain_status_type")]
pub enum BlockStatus {
    #[postgres(name = "pending")]
    Pending,
    #[postgres(name = "canonical")]
    Canonical,
    #[postgres(name = "orphaned")]
    Orphaned,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ResponseEntity {
    pub signals: Vec<DBResponse>
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, FromSql, Serialize, Deserialize)]
pub enum Status {
    Settled,
    Unsettled,
    Invalid
}