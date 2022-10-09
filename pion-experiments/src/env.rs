use std::fmt;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarIndex(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarLevel(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnvLen(pub usize);

impl EnvLen {
    pub fn next(self) -> Self { Self(self.0 + 1) }

    pub fn to_level(self) -> VarLevel { VarLevel(self.0) }

    pub fn index_to_level(self, index: VarIndex) -> Option<VarLevel> {
        Some(VarLevel(self.0.checked_sub(index.0)?.checked_sub(1)?))
    }

    pub fn level_to_index(self, level: VarLevel) -> Option<VarIndex> {
        Some(VarIndex(self.0.checked_sub(level.0)?.checked_sub(1)?))
    }
}

#[derive(Clone, PartialEq, Eq, Default)]
pub struct Env<T> {
    entries: rpds::Vector<T>,
}

impl<T: fmt::Debug> fmt::Debug for Env<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.entries.iter()).finish()
    }
}

impl<T> Env<T> {
    pub fn new() -> Self {
        Self {
            entries: rpds::Vector::new(),
        }
    }

    pub fn len(&self) -> EnvLen { EnvLen(self.entries.len()) }

    pub fn is_empty(&self) -> bool { self.len().0 == 0 }

    pub fn get(&self, var: VarIndex) -> Option<&T> {
        let index = self.entries.len().checked_sub(var.0)?.checked_sub(1)?;
        self.entries.get(index)
    }
}

/// Immutable methods
impl<T> Env<T> {
    pub fn pushed(&self, elem: T) -> Self {
        Self {
            entries: self.entries.push_back(elem),
        }
    }

    pub fn extended(&self, other: Self) -> Self
    where
        T: Clone,
    {
        let mut entries = self.entries.clone();
        entries.extend(other.entries.into_iter().cloned());
        Self { entries }
    }
}

/// Mutable methods
impl<T> Env<T> {
    pub fn push(&mut self, elem: T) { self.entries.push_back_mut(elem); }

    pub fn pop(&mut self) { self.entries.drop_last_mut(); }
}
