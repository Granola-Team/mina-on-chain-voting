use axum::Router;
use std::net::SocketAddr;
use tokio::signal;

pub mod keyword;

pub(crate) trait Build {
    fn build() -> Router;
}

impl Build for Router {
    fn build() -> Router {
        keyword::router()
    }
}

pub(crate) async fn serve(router: axum::Router, port: u16) {
    let addr = SocketAddr::from(([127, 0, 0, 1], port));
    axum::Server::bind(&addr)
        .serve(router.into_make_service())
        .with_graceful_shutdown(shutdown_signal())
        .await
        .expect("Error: failed to start axum runtime");
}

async fn shutdown_signal() {
    let windows = async {
        signal::ctrl_c()
            .await
            .expect("Error: failed to install shutdown handler");
    };

    #[cfg(unix)]
    let unix = async {
        signal::unix::signal(signal::unix::SignalKind::terminate())
            .expect("Error: failed to install shutdown handler")
            .recv()
            .await;
    };

    #[cfg(not(unix))]
    let terminate = std::future::pending::<()>();

    tokio::select! {
        _ = windows => {},
        _ = unix => {},
    }

    println!("Signal received - starting graceful shutdown...");
}
