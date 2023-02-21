use axum::Router;

mod proposal;

pub(crate) trait Build {
    fn build() -> Router;
}

impl Build for Router {
    fn build() -> Router {
        proposal::router()
    }
}
