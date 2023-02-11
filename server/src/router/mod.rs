pub mod keyword;

use crate::prelude::*;

use super::Config;
use axum::{routing::get, Router};

pub enum Version {
    V1,
}

pub fn prefix(path: impl Into<String>, version: Version) -> String {
    let path = path.into();
    match version {
        Version::V1 => f!("/api/v1/{path}"),
    }
}

pub trait Build {
    fn build(cfg: &Config) -> Router;
}

impl Build for Router {
    fn build(cfg: &Config) -> Router {
        Router::new()
            .route(
                &prefix(":keyword", Version::V1),
                get(keyword::keyword_handler),
            )
            .route(
                &prefix(":keyword/results", Version::V1),
                get(keyword::keyword_results_handler),
            )
    }
}
