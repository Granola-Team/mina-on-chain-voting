use serde::Deserialize;

use super::{DBConnectionManager, FetchTransactionResult};

pub struct MockConnectionManager;

#[derive(Deserialize)]
struct TxGQLSource {
    _account: String,
}
#[derive(Deserialize)]
struct TxGQLResult {
    #[serde(rename = "blockHeight")]
    _block_height: i64,
    _canonical: bool,
    #[serde(rename = "dateTime")]
    _date_time: String,
    _hash: String,
    _memo: String,
    _nonce: i64,
    _source: TxGQLSource,
}

impl From<TxGQLResult> for FetchTransactionResult {
    fn from(_value: TxGQLResult) -> Self {
        todo!()
    }
}

impl DBConnectionManager for MockConnectionManager {
    fn fetch_chain_tip(&self) -> crate::prelude::Result<i64> {
        Ok(211012)
    }

    fn fetch_latest_slot(&self) -> crate::prelude::Result<i64> {
        Ok(211302)
    }

    fn fetch_transactions(
        &self,
        _start_time: i64,
        _end_time: i64,
    ) -> crate::prelude::Result<Vec<super::FetchTransactionResult>> {
        todo!()
    }

    fn fetch_mina_proposals(
        &self,
    ) -> crate::prelude::Result<Vec<crate::models::diesel::MinaProposal>> {
        todo!()
    }

    fn fetch_mina_proposal(
        &self,
        _path: i32,
    ) -> crate::prelude::Result<crate::models::diesel::MinaProposal> {
        todo!()
    }
}
