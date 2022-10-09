#![feature(once_cell)]
#![warn(clippy::all, clippy::nursery, unused_qualifications)]
#![allow(clippy::missing_const_for_fn, clippy::option_if_let_else)]

pub mod env;
pub mod semantics;
pub mod surface;
pub mod typecheck;
