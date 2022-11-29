use axum::{Router, http::Method, Extension};
use tower::ServiceBuilder;
use tower_http::cors::{CorsLayer, Any};

use crate::ApiContext;

use super::Build;

pub mod keyword;

pub async fn build_router(context: ApiContext) -> Router {
    let cors = CorsLayer::new()
    .allow_methods([Method::GET, Method::POST])
    .allow_origin(Any);
    axum::Router::build_v1(&context.config)
    .layer(ServiceBuilder::new().layer(cors).layer(Extension(context)))
}