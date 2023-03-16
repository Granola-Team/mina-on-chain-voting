use diesel::prelude::*;
use diesel::r2d2::ConnectionManager;
use diesel::r2d2::Pool;

use crate::config::Config;

pub(crate) mod archive;
pub(crate) mod cache;

pub(crate) type PgConnectionPool = Pool<ConnectionManager<PgConnection>>;

pub(crate) struct DBConnectionManager {
    pub(crate) main: PgConnectionPool,
    pub(crate) archive: PgConnectionPool,
}

impl DBConnectionManager {
    pub(crate) fn get_connections(cfg: &Config) -> DBConnectionManager {
        let main_manager = ConnectionManager::<PgConnection>::new(&cfg.database_url);
        let archive_manager = ConnectionManager::<PgConnection>::new(&cfg.archive_database_url);

        DBConnectionManager {
            main: Pool::builder()
                .test_on_check_out(true)
                .build(main_manager)
                .unwrap_or_else(|_| panic!("Error: failed to build `main` connection pool")),

            archive: Pool::builder()
                .test_on_check_out(true)
                .build(archive_manager)
                .unwrap_or_else(|_| panic!("Error: failed to build `archive` connection pool")),
        }
    }
}
