pub mod api;

use super::Config;
use axum::{
    http::StatusCode,
    routing::{get, get_service},
    Router,
};

use axum_extra::routing::SpaRouter;
use serde::{Deserialize, Serialize};
use tower_http::services::ServeFile;
pub trait Build {
    fn build_v1(cfg: &Config) -> Router;
}

impl Build for Router {
    fn build_v1(cfg: &Config) -> Router {
        let spa = SpaRouter::new("/assets", format!("{}/assets", &cfg.client_path)).index_file("index.html");
        let react_router_fallback =
            get_service(ServeFile::new(format!("{}/index.html", &cfg.client_path))).handle_error(
                |error: std::io::Error| async move {
                    (
                        StatusCode::INTERNAL_SERVER_ERROR,
                        format!("Internal Server Error: {}", error),
                    )
                },
            );

        Router::new()
            .merge(spa)
            .route("/api/v1/:keyword", get(api::handler))
            .fallback(react_router_fallback)
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum QueryRequestFilter {
    Mainnet,
    Devnet,
}
