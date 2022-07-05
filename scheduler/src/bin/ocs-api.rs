extern crate serde_derive;

use on_chain_signalling_api::handlers;

use actix_web::{
    App, HttpServer, web::Data,
};

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    dotenv::dotenv().ok();
    
    use tokio_postgres::{NoTls};
    let (client, connection) =
        tokio_postgres::connect("host=localhost user=postgres", NoTls).await.unwrap();
        
    tokio::spawn(async move {
        if let Err(e) = connection.await {
            eprintln!("connection error: {}", e);
        }
    });
    let arc_client = std::sync::Arc::new(client);

    HttpServer::new(move || {
        App::new()
            .app_data(Data::new(arc_client.clone()))
            .service(handlers::votes)
    })
        .bind(("127.0.0.1", 8080))?
        .run()
        .await
}
