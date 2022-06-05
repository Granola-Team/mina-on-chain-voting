use chrono::Local;
use std::error::Error;
use tokio::time::{sleep, Duration};
// use JobSchedule;
use mina_graphql_rs::consts::*;
use mina_graphql_rs::scheduler::*;
use mina_graphql_rs::*;
use std::collections::HashMap;
use anyhow::{anyhow, bail, Result};
#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    println!("start {}", Local::now().time());
    let schedule: JobSchedule = JobSchedule::new();
    // let mut payments_performed_for_address;
    schedule
        .add(
            "1/10 * * * * *".to_string(),
            Box::new(|_i| {
                Box::pin(async move {
               // let percentages = get_staking_percentages("B62qrHzjcZbYSsrcXVgGko7go1DzSEBfdQGPon5X4LEGExtNJZA4ECj", 0).await.unwrap();
               // println!("{:?}", percentages); 
             
               let payments_performed = get_total_payments_performed_single("B62qpG8jpefptAeT9en6WoPzaW312rMJLwoL8pRmqcS8FCKiixRjofg", "B62qrHzjcZbYSsrcXVgGko7go1DzSEBfdQGPon5X4LEGExtNJZA4ECj").await;
               // payments_performed_for_address.get_mut("B62qpG8jpefptAeT9en6WoPzaW312rMJLwoL8pRmqcS8FCKiixRjofg").ok_or(anyhow!("should have gotten address in map")) += payments_performed; 
           //    let diffs = verify_enough_payments(
             //        8,
               //     "B62qrHzjcZbYSsrcXVgGko7go1DzSEBfdQGPon5X4LEGExtNJZA4ECj",
                 //   &[
                   //    "B62qpG8jpefptAeT9en6WoPzaW312rMJLwoL8pRmqcS8FCKiixRjofg".to_string(),
                     //  "B62qnzD7DZ5jci5vHrKCuJhmoHjwrM4pAhuAiKtQBD38h3SRKgixaV8".to_string(),
                    //   "B62qmwgXnydnYAxhFLbyZ68zDQsF2yxDR92UUucBJY2i2Mr3U4Qw2KG".to_string(),
                   //    "B62qmSLcBAgGJYa14CUyGdoZywpKuztSKWRJsnKFSxg3oeLAYaotLFv".to_string(),
                  //  ],
                 //   &["B62qkajGVuoMY9gc7wvnHhjgCab2XrCaphS5NDfCgMUkFZif6Q37EtE".to_string()],
            //  )
            //  .await
            //  .unwrap();
               println!("Payments Performed {:?}", payments_performed);                 
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
