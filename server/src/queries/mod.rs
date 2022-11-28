use std::{collections::VecDeque, env::VarError, sync::Arc};

use crate::{
    ledger::{HasConnectionAsync, Ledger},
    models::{BlockStatus, DBResponse},
    router::{Build, QueryRequestFilter},
    ApiContext, Config, SubCommand,
};

use anyhow::Context;
use axum::{http::Method, Extension, Router};
use mockall::predicate::*;
use mockall::*;
use sqlx::postgres::PgPoolOptions;
use tower::ServiceBuilder;
use tower_http::cors::{Any, CorsLayer};

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

pub fn create_config() -> Option<Config> {
    dotenv::dotenv().ok();
    if let Ok(mut env_vars) = vec![
        "MAINNET_DATABASE_URL",
        "DEVNET_DATABASE_URL",
        "MAINNET_LEDGER_PATH",
        "DEVNET_LEDGER_PATH",
    ]
    .into_iter()
    .map(std::env::var)
    .collect::<Result<VecDeque<String>, VarError>>()
    {
        let config = Config {
            mainnet_database_url: env_vars
                .pop_front()
                .unwrap_or_else(|| panic!("Error: Missing MAINNET_DATABASE_URL")),
            devnet_database_url: env_vars
                .pop_front()
                .unwrap_or_else(|| panic!("Error: Missing DEVNET_DATABASE_URL")),
            client_path: "../client/build".to_string(),
            mainnet_ledger_path: env_vars
                .pop_front()
                .unwrap_or_else(|| panic!("Error: Missing MAINNET_LEDGER")),
            devnet_ledger_path: env_vars
                .pop_front()
                .unwrap_or_else(|| panic!("Error: Missing DEVNET_LEDGER")),
            subcmd: SubCommand::Start,
        };

        return Some(config);
    }
    println!("Unable to test api context, secrets missing!");
    None
}

#[cfg(test)]
mod tests {
    use std::{collections::VecDeque, env::VarError};

    use crate::{router::QueryRequestFilter, ApiContext, Config, SubCommand};

    use super::create_config;

    #[tokio::test]
    pub async fn api_context_new() {
        if let Some(config) = create_config() {
            let ctx = ApiContext::new(config).await;
            assert_ok::assert_ok!(ctx);
        }
    }

    #[tokio::test]
    pub async fn api_context_get_latest_block_height_mainnet() {
        if let Some(config) = create_config() {
            let ctx = ApiContext::new(config).await.unwrap();

            let latest_height = ctx
                .get_latest_block_height(&QueryRequestFilter::Mainnet)
                .await;
            assert_ok::assert_ok!(latest_height);
        }
    }

    #[tokio::test]
    pub async fn api_context_get_signals_mainnet() {
        if let Some(config) = create_config() {
            let ctx = ApiContext::new(config).await.unwrap();

            let signals = ctx.get_signals(&QueryRequestFilter::Mainnet).await;
            assert_ok::assert_ok!(signals);
        }
    }

    #[tokio::test]
    pub async fn api_context_get_latest_block_height_devnet() {
        if let Some(config) = create_config() {
            let ctx = ApiContext::new(config).await.unwrap();

            let latest_height = ctx
                .get_latest_block_height(&QueryRequestFilter::Devnet)
                .await;
            assert_ok::assert_ok!(latest_height);
        }
    }

    #[tokio::test]
    pub async fn api_context_get_signals_devnet() {
        if let Some(config) = create_config() {
            let ctx = ApiContext::new(config).await.unwrap();

            let signals = ctx.get_signals(&QueryRequestFilter::Devnet).await;
            assert_ok::assert_ok!(signals);
        }
    }
}
