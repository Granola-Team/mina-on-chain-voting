pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    IOError(std::io::Error),
    EnvVarError(std::env::VarError),
    PostgresError(tokio_postgres::Error),
    JoinError(tokio::task::JoinError),
    IntegerParsingError(std::num::ParseIntError),
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Error::IOError(err)
    }
}

impl From<std::env::VarError> for Error {
    fn from(err: std::env::VarError) -> Self {
        Error::EnvVarError(err)
    }
}

impl From<tokio_postgres::Error> for Error {
    fn from(err: tokio_postgres::Error) -> Self {
        Error::PostgresError(err)
    }
}

impl From<tokio::task::JoinError> for Error {
    fn from(e: tokio::task::JoinError) -> Self {
        Error::JoinError(e)
    }
}

impl From<std::num::ParseIntError> for Error {
    fn from(e: std::num::ParseIntError) -> Self {
        Error::IntegerParsingError(e)
    }
}
