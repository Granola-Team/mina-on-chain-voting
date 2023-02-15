use axum::Router;

mod keyword;

pub(crate) trait Build {
    fn build() -> Router;
}

impl Build for Router {
    fn build() -> Router {
        keyword::router()
    }
}
