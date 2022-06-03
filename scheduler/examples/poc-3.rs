use tokio::time::{sleep, Duration};
use tokio::sync::Mutex;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

pub type Callback = Box<dyn Fn(usize) -> Pin<Box<dyn Future<Output=()> + Send>> + Send + Sync>;

#[tokio::main]
async fn main() {
    let futures: Arc<Mutex<Vec<Arc<Mutex<Callback>>>>> = Arc::new(Mutex::new(vec![]));

    futures.lock().await.push(Arc::new(Mutex::new(Box::new(|i| Box::pin(async move {
        sleep(Duration::from_secs(1)).await;
        println!("One second passed! {}", i);
    })))));

    futures.lock().await.push(Arc::new(Mutex::new(Box::new(|i| Box::pin(async move {
        sleep(Duration::from_secs(2)).await;
        println!("Two second passed! {}", i);
    })))));

    futures.lock().await.push(Arc::new(Mutex::new(Box::new(|i| Box::pin(async move {
        sleep(Duration::from_secs(60)).await;
        println!("60 second passed! {}", i);
    })))));

    tokio::spawn(async move {
        let lock = futures.lock().await;

        loop {
            for (i, fut) in (*lock).iter().enumerate() {
                let clone = Arc::clone(fut);

                tokio::spawn(async move {
                    (clone.lock().await)(i).await;
                });
            }
        }

    }).await.ok();
}