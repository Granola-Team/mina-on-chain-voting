use anyhow::Context;
use diesel::r2d2::ConnectionManager;
use diesel::sql_types::BigInt;
use diesel::ExpressionMethods;
use diesel::{sql_query, PgConnection, QueryDsl, RunQueryDsl};
use r2d2::Pool;

use super::{FetchChainTipResult, FetchLatestSlotResult, FetchTransactionResult};
use crate::config::Config;
use crate::database::DBConnectionManager;
use crate::models::diesel::MinaProposal;
use crate::prelude::*;

pub(crate) type PgConnectionPool = Pool<ConnectionManager<PgConnection>>;

pub(crate) struct PostgresConnectionManager {
    pub(crate) main: PgConnectionPool,
    pub(crate) archive: PgConnectionPool,
}

impl PostgresConnectionManager {
    pub fn get_connections(cfg: &Config) -> Self {
        let main_manager = ConnectionManager::<PgConnection>::new(&cfg.database_url);
        let archive_manager = ConnectionManager::<PgConnection>::new(&cfg.archive_database_url);

        PostgresConnectionManager {
            main: Pool::builder()
                .test_on_check_out(true)
                .build(main_manager)
                .unwrap_or_else(|_| panic!("Error: failed to build `main` connection pool")),

            archive: Pool::builder()
                .test_on_check_out(true)
                .build(archive_manager)
                .unwrap_or_else(|_| panic!("Error: failed to build `archive` connection pool")),
        }
    }
}

impl DBConnectionManager for PostgresConnectionManager {
    fn fetch_chain_tip(&self) -> Result<i64> {
        let mut connection = self
            .archive
            .get()
            .context("failed to get archive db connection")?;

        let result = sql_query("SELECT MAX(height) FROM blocks")
            .get_result::<FetchChainTipResult>(&mut connection)?;
        Ok(result.max)
    }

    fn fetch_latest_slot(&self) -> Result<i64> {
        let connection = &mut self
            .archive
            .get()
            .context("failed to get archive db connection")?;

        let result = sql_query("SELECT MAX(global_slot) FROM blocks")
            .get_result::<FetchLatestSlotResult>(connection)?;
        Ok(result.max)
    }

    fn fetch_transactions(
        &self,
        start_time: i64,
        end_time: i64,
    ) -> Result<Vec<FetchTransactionResult>> {
        let connection = &mut self
            .archive
            .get()
            .context("failed to get archive db connection")?;

        let results = sql_query(
            "SELECT DISTINCT pk.value as account, uc.memo as memo, uc.nonce as nonce, uc.hash as hash, b.height as height, b.chain_status as status, b.timestamp as timestamp
            FROM user_commands AS uc
            JOIN blocks_user_commands AS buc
            ON uc.id = buc.user_command_id
            JOIN blocks AS b
            ON buc.block_id = b.id
            JOIN public_keys AS pk
            ON uc.source_id = pk.id
            WHERE uc.type = 'payment'
            AND uc.source_id = uc.receiver_id
            AND uc.token = 1
            AND NOT b.chain_status = 'orphaned'
            AND buc.status = 'applied'
            AND b.timestamp BETWEEN $1 AND $2"
            );
        let results = results
            .bind::<BigInt, _>(start_time)
            .bind::<BigInt, _>(end_time)
            .get_results(connection)?;
        Ok(results)
    }

    fn fetch_mina_proposals(&self) -> Result<Vec<MinaProposal>> {
        use crate::schema::mina_proposals::dsl as mina_proposal_dsl;
        let connection = &mut self
            .main
            .get()
            .context("failed to get primary db connection")?;

        Ok(mina_proposal_dsl::mina_proposals
            .order(mina_proposal_dsl::id.desc())
            .load(connection)?)
    }

    fn fetch_mina_proposal(&self, id: i32) -> Result<crate::models::diesel::MinaProposal> {
        use crate::schema::mina_proposals::dsl as mina_proposal_dsl;
        let connection = &mut self
            .main
            .get()
            .context("failed to get primary db connection")?;

        Ok(mina_proposal_dsl::mina_proposals
            .find(id)
            .first(connection)?)
    }
}
