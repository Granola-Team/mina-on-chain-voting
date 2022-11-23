use std::sync::Arc;

use crate::{
    models::{BlockStatus, DBResponse, QueryRequestFilter},
    router::Build,
    ApiContext, Config, ledger::{Ledger, HasConnectionAsync},
};

use axum::{http::Method, Extension, Router};
use mockall::predicate::*;
use mockall::*;
use sqlx::postgres::PgPoolOptions;
use tower::ServiceBuilder;
use tower_http::cors::{Any, CorsLayer};
use anyhow::Context;

#[automock]
impl ApiContext {
    pub async fn new(config: Config) -> anyhow::Result<Self> {
        let mainnet_ledger = Ledger::init_async(&config.mainnet_ledger_path)
            .await
            .expect("Error: Could not create mainnet ledger.");

        let devnet_ledger = Ledger::init_async(&config.devnet_ledger_path)
            .await
            .expect("Error: Could not create devnet ledger.");

        let mainnet_db = PgPoolOptions::new()
            .max_connections(25)
            .connect(&config.mainnet_database_url)
            .await
            .context("Error: Could not connect to mainnet database.")?;

        let devnet_db = PgPoolOptions::new()
            .max_connections(25)
            .connect(&config.devnet_database_url)
            .await
            .context("Error: Could not connect to devnet database.")?;

        Ok(ApiContext {
            config: Arc::new(config),
            mainnet_ledger: Arc::new(mainnet_ledger.db),
            devnet_ledger: Arc::new(devnet_ledger.db),
            mainnet_db,
            devnet_db,
        })
    }

    pub async fn get_latest_block_height(
        &self,
        network: &QueryRequestFilter,
    ) -> Result<i64, sqlx::Error> {
        let row: (i64,) = sqlx::query_as("SELECT MAX(height) FROM blocks;")
            .fetch_one(match network {
                QueryRequestFilter::Mainnet => &self.mainnet_db,
                _ => &self.devnet_db,
            })
            .await?;
        Ok(row.0)
    }

    /// By default we get all signals every query.
    /// This is wildly inefficient & we should work towards an alternative.
    /// Since we're essentially only interested in memo's - maybe a way to decode base58 on the DB side?
    pub async fn get_signals(
        &self,
        network: &QueryRequestFilter,
    ) -> Result<Vec<DBResponse>, sqlx::Error> {
        let db = match network {
            QueryRequestFilter::Mainnet => &self.mainnet_db,
            QueryRequestFilter::Devnet => &self.devnet_db,
        };

        sqlx::query_as!(DBResponse,
        r#"
        SELECT DISTINCT pk.value as account, uc.memo as memo, uc.nonce as nonce, b.height as height, b.chain_status as "status: BlockStatus", b.timestamp as timestamp
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
        "#
        ).fetch_all(db).await
    }
}

pub async fn build_router(context: ApiContext) -> Router {
    let cors = CorsLayer::new()
        .allow_methods([Method::GET, Method::POST])
        .allow_origin(Any);
    axum::Router::build_v1(&context.config)
        .layer(ServiceBuilder::new().layer(cors).layer(Extension(context)))
}
