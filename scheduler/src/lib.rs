#![deny(missing_docs)]

//! CRON scheduler using Tokio for async jobs.

use std::future::Future;
use std::pin::Pin;
use std::str::FromStr;
use std::sync::Arc;
use std::collections::HashMap;  

use chrono::{DateTime, Local};
use cron::Schedule;
use uuid::Uuid;
use tokio::sync::Mutex;
use tokio::task::JoinHandle;
use tokio::time::{sleep, Duration};

/// Used to represent a thread safe function, containing an async block
type Callback = Box<dyn Fn(Uuid) -> Pin<Box<dyn Future<Output = ()> + Send>> + Send + Sync>;

struct Job {
    callback: Callback,
    schedule: Schedule,
    next_run: DateTime<Local>,
    uuid: Uuid,
}

impl Job {
    pub fn new(schedule: Schedule, callback: Callback) -> Result<Self, &'static str> {
        match schedule
            .after(&Local::now())
            .take(1)
            .collect::<Vec<DateTime<Local>>>()
            .get(0)
        {
            Some(next_run) => Ok(Job {
                callback,
                schedule,
                next_run: next_run.clone(),
                uuid: Uuid::new_v4(),
            }),
            None => Err("The next run is scheduled in the past"),
        }
    }

    pub async fn run(&mut self) {
        self.next_run = *self
            .schedule
            .after(&self.next_run)
            .take(1)
            .collect::<Vec<DateTime<Local>>>()
            .get(0)
            .unwrap();
        (self.callback)(self.uuid).await
    }

    pub fn should_run(&self) -> bool {
        self.next_run < Local::now()
    }

    pub fn get_uuid(&self) -> Uuid {
        self.uuid
    }
}


/// CRON job scheduler.
///
/// Allows adding, removing and running CRN jobs.
///
/// # Examples
///
/// ```
///
/// // A new instance is ready for use:
/// let schedule: JobSchedule = JobSchedule::new();
/// ```
pub struct JobSchedule(Arc<Mutex<HashMap<Uuid, Arc<Mutex<Job>>>>>);

impl JobSchedule {

    /// Create a new instance of `JobSchedule`
    pub fn new() -> Self {
        JobSchedule(Arc::new(Mutex::new(HashMap::new())))
    }

    /// Add a new CRON job using a callback with async code.
    pub async fn add(
        &self,
        cron_pattern: String,
        job_cb: Callback,
    ) -> Result<Uuid, &'static str> {
        match Schedule::from_str(cron_pattern.as_str()) {
            Ok(sched) => match Job::new(sched, job_cb) {
                Ok(job) => {
                    let uuid = job.get_uuid();
                    self.0.lock().await.insert(uuid, Arc::new(Mutex::new(job)));
                    Ok(uuid)
                }
                Err(err) => Err(err),
            },
            Err(_err) => Err("Failed to parse CRON pattern string"),
        }
    }

    /// Remove a job from the schedule by it's UUID.
    pub async fn remove(&self, uuid: Uuid) -> Result<(), &'static str> {
        match self.0.lock().await.remove(&uuid) {
            Some(_job) => Ok(()),
            None => Err("boo"),
        }
    }

    /// Start the schedule runner.
    pub fn run(&self) -> JoinHandle<()> {
        let clone = Arc::clone(&self.0);

        tokio::spawn(async move {
            let mut lock = clone.lock().await;
            loop {
                sleep(Duration::from_millis(300)).await;
                if !lock.is_empty() {
                    for (_uuid, job) in (*lock).iter_mut() {
                        let job_clone = Arc::clone(&job);
                        if job_clone.lock().await.should_run() {
                            tokio::spawn(async move {
                                job_clone.lock().await.run().await;
                            });
                        }
                    }
                }
            }
        })
    }
}
