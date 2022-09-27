use chrono::Datelike;
use clap::Parser;
use std::error::Error;
use std::path::Path;
use std::process::Command;
use tokio_postgres::NoTls;

extern crate dotenv;

#[derive(clap::Parser)]
struct Config {
    #[clap(long, env)]
    pub mainnet_database_url: String,
    #[clap(long, env)]
    pub devnet_database_url: String,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    dotenv::dotenv().ok();
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));

    loop {
        let config = Config::parse();
        task(&config).await;
        log::info!("Finished task - sleeping for 5 minutes.");
        tokio::time::sleep(std::time::Duration::from_secs(300)).await;
    }
}

async fn download_file(target: String, name: &str, network: String) -> anyhow::Result<()> {
    match Path::new(&format!("./temp/{}", name)).exists() {
        true => {
            let yesterday = chrono::Utc::now() - chrono::Duration::days(1);
            let day = yesterday.day();
            let year = yesterday.year();
            let month = match yesterday.month() {
                1..=9 => format!("0{}", yesterday.month()),
                _ => format!("{}", yesterday.month()),
            };

            let path = format!(
                "./temp/{}-archive-dump-{}-{}-{}_0000.sql.tar.gz",
                network, year, month, day
            );

            match Path::new(&path).exists() {
                true => std::fs::remove_file(path).unwrap(),
                _ => (),
            }

            Err(anyhow::Error::msg(
                "File already exists & may have been imported previously.",
            ))
        }
        false => {
            println!("Downloading SQL Dump: {}", target);

            let mut curl = Command::new("curl")
                .arg(&target)
                .arg("-O")
                .current_dir("./temp")
                .spawn()
                .unwrap();

            match curl.wait().unwrap().success() {
                true => Ok(()),
                false => Err(anyhow::Error::msg(
                    "An issue occured while downloading file.",
                )),
            }
        }
    }
}

async fn update_database(config: &Config, network: &str, name: String) -> anyhow::Result<()> {
    let db = match network {
        "mainnet" => config.mainnet_database_url.clone(),
        "devnet" => config.devnet_database_url.clone(),
        &_ => String::default(),
    };

    let (client, _) = tokio_postgres::connect(&db, NoTls).await?;

    client
        .query("DROP DATABASE IF EXISTS $1", &[&network])
        .await?;

    let mut update_db = Command::new("psql")
        .arg(db)
        .arg(format!("< {}", name))
        .spawn()
        .unwrap();

    match update_db.wait().unwrap().success() {
        true => {
            client
                .query(
                    "ALTER DATABASE archive_balances_migrated RENAME TO $1;",
                    &[&network],
                )
                .await?;
            Ok(())
        }
        false => Err(anyhow::Error::msg(
            "An issue occured while updating the database.",
        )),
    }
}

async fn task(config: &Config) -> anyhow::Result<()> {
    let current_date = chrono::Utc::now();
    let day = current_date.day();
    let year = current_date.year();
    let month = match current_date.month() {
        1..=9 => format!("0{}", current_date.month()),
        _ => format!("{}", current_date.month()),
    };

    let networks = vec!["mainnet", "devnet"];

    for network in networks {
        let name = format!(
            "{}-archive-dump-{}-{}-{}_0000.sql.tar.gz",
            network, year, month, day
        );

        let target = format!("https://storage.googleapis.com/mina-archive-dumps/{}", name);

        match download_file(target, &name, network.to_string()).await {
            Ok(_) => {
                update_database(config, network, name).await.unwrap();
            }
            Err(err) => {
                log::info!("Error: {}", err)
            }
        }
    }

    Ok(())
}
