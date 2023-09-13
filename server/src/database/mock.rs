use serde::Deserialize;

use crate::models::vote::MinaBlockStatus;

use super::{DBConnectionManager, FetchTransactionResult};

pub struct MockConnectionManager;

#[derive(Deserialize)]
struct TxGQLSource {
    account: String,
}
#[derive(Deserialize)]
struct TxGQLResult {
    #[serde(rename = "blockHeight")]
    block_height: i64,
    canonical: bool,
    #[serde(rename = "dateTime")]
    date_time: String,
    hash: String,
    memo: String,
    nonce: i64,
    source: TxGQLSource,
}

impl Into<FetchTransactionResult> for TxGQLResult {
    fn into(self) -> FetchTransactionResult {
        FetchTransactionResult {
            account: self.source.account,
            hash: self.hash,
            memo: self.memo,
            height: self.block_height,
            status: if self.canonical {
                MinaBlockStatus::Canonical
            } else {
                MinaBlockStatus::Pending
            },
            timestamp: 0,
            nonce: self.nonce,
        }
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
        start_time: i64,
        end_time: i64,
    ) -> crate::prelude::Result<Vec<super::FetchTransactionResult>> {
        Ok(vec![FetchTransactionResult {
            account: todo!(),
            hash: todo!(),
            memo: todo!(),
            height: todo!(),
            status: todo!(),
            timestamp: todo!(),
            nonce: todo!(),
        }])
    }

    fn fetch_mina_proposals(
        &self,
    ) -> crate::prelude::Result<Vec<crate::models::diesel::MinaProposal>> {
        todo!()
    }

    fn fetch_mina_proposal(
        &self,
        path: i32,
    ) -> crate::prelude::Result<crate::models::diesel::MinaProposal> {
        todo!()
    }
}
