#![warn(clippy::all, clippy::nursery, unused_qualifications)]
#![allow(
    clippy::missing_const_for_fn,
    clippy::new_without_default,
    clippy::option_if_let_else
)]

use std::sync::{Arc, Mutex};

use salsa::DebugWithDb;

pub mod core;
pub mod ir;
pub mod surface;

#[salsa::jar(db = Db)]
pub struct Jar(
    crate::ir::symbol::Symbol,
    crate::ir::input_file::InputFile,
    //
);

pub trait Db: salsa::DbWithJar<Jar> {}

impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}

#[derive(Default)]
#[salsa::db(Jar)]
pub struct Database {
    storage: salsa::Storage<Self>,
    log: Option<Arc<Mutex<Vec<String>>>>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FileId(usize);

impl salsa::Database for Database {
    fn salsa_event(&self, event: salsa::Event) {
        // Log interesting events, if logging is enabled
        if let Some(log) = &self.log {
            // don't log boring events
            if let salsa::EventKind::WillExecute { .. } = event.kind {
                log.lock()
                    .unwrap()
                    .push(format!("Event: {:?}", event.debug(self)));
            }
        }
    }
}

impl salsa::ParallelDatabase for Database {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(Self {
            storage: self.storage.snapshot(),
            log: self.log.clone(),
        })
    }
}
