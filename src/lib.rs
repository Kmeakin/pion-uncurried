#![warn(clippy::all, clippy::nursery, unused_qualifications)]
#![allow(
    clippy::missing_const_for_fn,
    clippy::new_without_default,
    clippy::option_if_let_else
)]

use std::sync::{Arc, Mutex};

use salsa::DebugWithDb;

pub mod cli;
pub mod core;
pub mod ir;
pub mod surface;

pub mod diagnostic;
pub mod file;
pub mod span;
pub mod symbol;

#[salsa::jar(db = Db)]
pub struct Jar(
    crate::file::File,
    crate::diagnostic::Diagnostics,
    crate::symbol::Symbol,
    // surface
    crate::surface::parser::parse_file,
    // ir
    crate::ir::syntax::Module,
    crate::ir::syntax::LetDef,
    crate::ir::syntax::EnumDef,
    crate::ir::syntax::EnumVariant,
    crate::ir::lower_file,
    crate::ir::lookup_item,
    // core
    crate::core::elab::elab_module,
    crate::core::elab::elab_let_def,
    crate::core::elab::synth_enum_def,
    crate::core::elab::elab_enum_def,
    crate::core::elab::elab_enum_variant,
);

pub trait Db: salsa::DbWithJar<Jar> {}

impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}

#[derive(Default)]
#[salsa::db(Jar)]
pub struct Database {
    storage: salsa::Storage<Self>,
    log: Option<Arc<Mutex<Vec<String>>>>,
}

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
