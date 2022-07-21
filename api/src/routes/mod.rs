pub mod votes;

use actix_web::web;
pub use votes::config as votes_config;

pub fn v1_config(cfg: &mut web::ServiceConfig) {
    cfg.service(web::scope("/api").configure(votes_config));
}