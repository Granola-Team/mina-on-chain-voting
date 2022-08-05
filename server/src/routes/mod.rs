pub mod keyword;

use actix_web::web;
pub use keyword::config as keyword_config;

pub fn v1_config(cfg: &mut web::ServiceConfig) {
    cfg.service(web::scope("/v1").configure(keyword_config));
}
