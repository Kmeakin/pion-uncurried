#![warn(clippy::all, clippy::nursery, unused_qualifications)]
#![allow(clippy::missing_const_for_fn, clippy::new_without_default)]

use std::rc::Rc;

pub mod cli;
pub mod core;
pub mod surface;

pub type RcStr = Rc<str>;
pub type FileId = usize;
