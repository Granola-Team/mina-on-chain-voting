use osc_api::{router::QueryRequestFilter, ApiContext, utils::create_config, queries::{get_latest_block_height, get_signals}};

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

        let latest_height = get_latest_block_height(&ctx, &QueryRequestFilter::Mainnet)
            .await;
        assert_ok::assert_ok!(latest_height);
    }
}

#[tokio::test]
pub async fn api_context_get_signals_mainnet() {
    if let Some(config) = create_config() {
        let ctx = ApiContext::new(config).await.unwrap();

        let signals = get_signals(&ctx, &QueryRequestFilter::Mainnet).await;
        assert_ok::assert_ok!(signals);
    }
}

#[tokio::test]
pub async fn api_context_get_latest_block_height_devnet() {
    if let Some(config) = create_config() {
        let ctx = ApiContext::new(config).await.unwrap();

        let latest_height = get_latest_block_height(&ctx, &QueryRequestFilter::Devnet)
            .await;
        assert_ok::assert_ok!(latest_height);
    }
}

#[tokio::test]
pub async fn api_context_get_signals_devnet() {
    if let Some(config) = create_config() {
        let ctx = ApiContext::new(config).await.unwrap();

        let signals = get_signals(&ctx, &QueryRequestFilter::Devnet).await;
        assert_ok::assert_ok!(signals);
    }
}
