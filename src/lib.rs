#![warn(clippy::all, clippy::nursery, unused_qualifications)]
#![allow(clippy::missing_const_for_fn, clippy::new_without_default)]
#![feature(custom_test_frameworks)]
#![test_runner(datatest::runner)]

use std::rc::Rc;

pub mod cli;
pub mod core;
pub mod surface;

#[cfg(test)]
mod tests;

pub type RcStr = Rc<str>;
pub type FileId = usize;
