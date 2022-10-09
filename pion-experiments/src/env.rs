use std::fmt;

use contracts::debug_requires;

use crate::semantics::RcValue;

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

#[derive(Clone, PartialEq, Eq)]
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

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &T> { self.entries.iter() }

    pub fn get(&self, var: VarIndex) -> Option<&T> {
        let index = self.entries.len().checked_sub(var.0)?.checked_sub(1)?;
        self.entries.get(index)
    }
}

impl<T> Default for Env<T> {
    fn default() -> Self { Self::new() }
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

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct LocalEnv {
    pub names: Env<String>,
    pub types: Env<RcValue>,
    pub values: Env<RcValue>,
}

impl LocalEnv {
    pub fn new() -> Self { Self::default() }

    pub fn len(&self) -> EnvLen { self.values.len() }

    #[debug_requires(self.names.len() == self.types.len())]
    #[debug_requires(self.names.len() == self.values.len())]
    pub fn pushed(&self, name: String, r#type: RcValue, value: RcValue) -> Self {
        let names = self.names.pushed(name);
        let types = self.types.pushed(r#type);
        let values = self.values.pushed(value);

        Self {
            names,
            values,
            types,
        }
    }

    pub fn lookup(&self, name: &str) -> Option<(VarIndex, &RcValue, &RcValue)> {
        let index = self
            .names
            .entries
            .iter()
            .rev()
            .enumerate()
            .find(|(_, n)| n.as_str() == name)
            .map(|(index, _)| VarIndex(index))?;
        let r#type = self.types.get(index)?;
        let value = self.types.get(index)?;
        Some((index, r#type, value))
    }
}
