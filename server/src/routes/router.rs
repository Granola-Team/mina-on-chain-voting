// skeleton for GQL router
// defined Context and Schema types

use warp::Filter;
use std::sync::Arc;
use juniper::http::graphiql::graphiql_source;

mod graphql {
    // Import graphql module

    pub(crate) fn graphql_handler(
        schema: Schema,
        context: Context,
        request: TransactionQueryVariables,
    ) -> Result<TransactionQueryResponseData, anyhow::Error> {
        // Call fetch_transactions_graphql and operations
    }
}

#[tokio::main]
async fn main() {
    // Initialize your context, schema, and other components

    let schema = Arc::new(schema);
    let context = Arc::new(context);

    let graphql_route = warp::post()
        .and(warp::path!("graphql"))
        .and(warp::any().map(move || Arc::clone(&schema)))
        .and(warp::any().map(move || Arc::clone(&context)))
        .and(warp::body::json())
        .and_then(graphql::graphql_handler);

    // other routes

    warp::serve(routes).run(([127, 0, 0, 1], 8000)).await;
}
