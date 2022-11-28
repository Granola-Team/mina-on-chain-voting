use osc_api::{queries::create_config, router::QueryRequestFilter, ApiContext};

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
