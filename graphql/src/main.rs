use chrono::Local;
use std::error::Error;
use tokio::time::{sleep, Duration};
// use JobSchedule;
use mina_graphql_rs::consts::*;
use mina_graphql_rs::scheduler::*;
use mina_graphql_rs::*;
use std::collections::HashMap;
use anyhow::{anyhow, bail, Result};
use postgres::{Client, NoTls};
use chrono::{DateTime, Utc, TimeZone};
struct Author {
    _id: i32,
    name: String,
    country: String
}

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
           
                let mut client = Client::connect("postgresql://postgres:@localhost:5432/mina_fork", NoTls).unwrap();

    // Create the table in the postgres Database.
    client.batch_execute("
        CREATE TABLE IF NOT EXISTS author (
            id              SERIAL PRIMARY KEY,
            name            VARCHAR NOT NULL,
            country         VARCHAR NOT NULL
            )
    ");

    client.batch_execute("
        CREATE TABLE IF NOT EXISTS book  (
            id              SERIAL PRIMARY KEY,
            title           VARCHAR NOT NULL,
            author_id       INTEGER NOT NULL REFERENCES author
            )
    ");
    let cur_time: DateTime<Utc> = Utc::now();
    

    // Insert the values to the tables in the Postgres database.
    let mut authors = HashMap::new();
    authors.insert(String::from("Pawan Bisht"), "United States");
    authors.insert(String::from("Pankaj Chaudhary "), "India");
    authors.insert(String::from("Lokesh Aggarwal"), "singapore");
    
    for (key, value) in &authors {
        let author = Author {
            _id: 0,
            name: key.to_string(),
            country: value.to_string()
        };

        client.execute(
            "INSERT INTO author (name, country) VALUES ($1, $2)",
            &[&author.name, &author.country],
        );
    }

    let account: &str = "";
    let voting: bool = false;

    // Retrieve the data from the table.
    for row in client.query("SELECT id, name, country FROM author", &[]).unwrap() {
        let author = Author {
            _id: row.get(0),
            name: row.get(1),
            country: row.get(2),
        };
        println!("Author {} is from {}", author.name, author.country);
    }


    // Delete the table from the Postgres database.
    client.batch_execute("
        DROP TABLE author, book;
    "); 
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
