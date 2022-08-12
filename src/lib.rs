#![warn(clippy::all, clippy::nursery, unused_qualifications)]
#![allow(clippy::missing_const_for_fn)]

use std::rc::Rc;

pub mod surface;

pub type RcStr = Rc<str>;
