use axum::Router;

mod info;
mod proposal;

pub(crate) trait Build {
    fn build() -> Router;
}

impl Build for Router {
    fn build() -> Router {
        proposal::router().merge(info::router())
    }
}
