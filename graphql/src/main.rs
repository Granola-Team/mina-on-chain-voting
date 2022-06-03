use chrono::Local;
use std::error::Error;
use tokio::time::{sleep, Duration};
// use JobSchedule;
use mina_graphql_rs::scheduler::*;
use mina_graphql_rs::*;
#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    println!("start {}", Local::now().time());
    let schedule: JobSchedule = JobSchedule::new();

    schedule
        .add(
            "1/10 * * * * *".to_string(),
            Box::new(|_i| {
                Box::pin(async move {
                let percentages = get_staking_percentages("B62qrHzjcZbYSsrcXVgGko7go1DzSEBfdQGPon5X4LEGExtNJZA4ECj", 0).await.unwrap();
                println!("{:?}", percentages); 
                    println!("1/10 {}", Local::now().time());
                    sleep(Duration::from_secs(10)).await;
                })
            }),
        )
        .await?;

    schedule
        .add(
            "1/20 * * * * *".to_string(),
            Box::new(|_i| {
                Box::pin(async move {
                    println!("1/20 {}", Local::now().time());
                    sleep(Duration::from_secs(20)).await;
                })
            }),
        )
        .await?;

    let _jh = tokio::spawn(schedule.run()).await?;

    Ok(())
}
