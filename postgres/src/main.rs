use postgres::{Client, NoTls, Error};
use std::collections::HashMap;

struct Author {
    _id: i32,
    name: String,
    country: String
}

fn main() -> Result<(), Error> {

    // Make connection with Postgres Database.
    let mut client = Client::connect("postgresql://postgres:postgres@localhost:5432/postgres", NoTls)?;

    // Create the table in the postgres Database.
    client.batch_execute("
        CREATE TABLE IF NOT EXISTS author (
            id              SERIAL PRIMARY KEY,
            name            VARCHAR NOT NULL,
            country         VARCHAR NOT NULL
            )
    ")?;

    client.batch_execute("
        CREATE TABLE IF NOT EXISTS book  (
            id              SERIAL PRIMARY KEY,
            title           VARCHAR NOT NULL,
            author_id       INTEGER NOT NULL REFERENCES author
            )
    ")?;

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
        )?;
    }

    // Retrieve the data from the table.
    for row in client.query("SELECT id, name, country FROM author", &[])? {
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
    ")?;

    Ok(())
}
