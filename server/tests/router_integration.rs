use axum::{body::Body, http::StatusCode};
use osc_api::{
    queries::{build_router, create_config},
    utils, ApiContext,
};

#[tokio::test]
pub async fn router_bad_network_param_gives_bad_request() {
    if let Some(config) = create_config() {
        let api_context = ApiContext::new(config).await.unwrap();
        let router = build_router(api_context).await;

        let response =
            utils::run_router_request(router, "/api/v1/magenta?network=badnet", Body::empty())
                .await;
        assert_eq!(response.status(), StatusCode::BAD_REQUEST)
    }
}

#[tokio::test]
pub async fn router_mainnet_gives_ok() {
    if let Some(config) = create_config() {
        let api_context = ApiContext::new(config).await.unwrap();
        let router = build_router(api_context).await;

        let response =
            utils::run_router_request(router, "/api/v1/magenta?network=Mainnet", Body::empty())
                .await;
        assert_eq!(response.status(), StatusCode::ACCEPTED)
    }
}

#[tokio::test]
pub async fn router_devnet_gives_ok() {
    if let Some(config) = create_config() {
        let api_context = ApiContext::new(config).await.unwrap();
        let router = build_router(api_context).await;

        let response =
            utils::run_router_request(router, "/api/v1/magenta?network=Devnet", Body::empty())
                .await;
        assert_eq!(response.status(), StatusCode::ACCEPTED)
    }
}
