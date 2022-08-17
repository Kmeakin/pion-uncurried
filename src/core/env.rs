use std::rc::Rc;

use contracts::debug_invariant;

use super::Value;
use crate::RcStr;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct VarIndex(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct VarLevel(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct EnvLen(pub usize);

impl EnvLen {
    pub fn index_to_level(&self, index: VarIndex) -> Option<VarLevel> {
        Some(VarLevel(self.0.checked_sub(index.0)?.checked_sub(1)?))
    }

    pub fn level_to_index(&self, level: VarLevel) -> Option<VarIndex> {
        Some(VarIndex(self.0.checked_sub(level.0)?.checked_sub(1)?))
    }

    pub fn push(&mut self) -> Self {
        let ret = *self;
        self.0 += 1;
        ret
    }

    pub fn pop(&mut self) -> Self {
        let ret = *self;
        self.0 -= 1;
        ret
    }

    pub fn extend(&mut self, other: Self) { self.0 += other.0; }

    pub fn truncate(&mut self, other: Self) { self.0 = other.0; }

    pub fn clear(&mut self) { self.0 = 0; }

    pub fn to_level(&self) -> VarLevel { VarLevel(self.0) }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UniqueEnv<T> {
    entries: Vec<T>,
}

impl<T> Default for UniqueEnv<T> {
    fn default() -> Self { Self::new() }
}

impl<T> UniqueEnv<T> {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    pub fn push(&mut self, entry: T) { self.entries.push(entry); }

    pub fn pop(&mut self) { self.entries.pop(); }

    pub fn truncate(&mut self, other: EnvLen) { self.entries.truncate(other.0); }

    pub fn iter_by_level(&self) -> impl DoubleEndedIterator<Item = (VarLevel, &T)> {
        self.entries
            .iter()
            .enumerate()
            .map(|(level, entry)| (VarLevel(level), entry))
    }

    pub fn iter_by_index(&self) -> impl DoubleEndedIterator<Item = (VarIndex, &T)> {
        self.entries
            .iter()
            .rev()
            .enumerate()
            .map(|(level, entry)| (VarIndex(level), entry))
    }

    pub fn len(&self) -> EnvLen { EnvLen(self.entries.len()) }

    pub fn get_by_index(&self, index: VarIndex) -> Option<&T> {
        self.get_by_level(self.len().index_to_level(index)?)
    }

    pub fn get_by_level(&self, level: VarLevel) -> Option<&T> { self.entries.get(level.0) }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SharedEnv<T> {
    entries: Rc<Vec<T>>,
}

impl<T: Clone> Default for SharedEnv<T> {
    fn default() -> Self { Self::new() }
}

impl<T: Clone> SharedEnv<T> {
    pub fn new() -> Self {
        Self {
            entries: Rc::new(Vec::new()),
        }
    }

    pub fn push(&mut self, entry: T) { Rc::make_mut(&mut self.entries).push(entry); }

    pub fn pop(&mut self) { Rc::make_mut(&mut self.entries).pop(); }

    pub fn truncate(&mut self, other: EnvLen) { Rc::make_mut(&mut self.entries).truncate(other.0); }

    pub fn iter_by_level(&self) -> impl DoubleEndedIterator<Item = (VarLevel, &T)> {
        self.entries
            .iter()
            .enumerate()
            .map(|(level, entry)| (VarLevel(level), entry))
    }

    pub fn iter_by_index(&self) -> impl DoubleEndedIterator<Item = (VarIndex, &T)> {
        self.entries
            .iter()
            .rev()
            .enumerate()
            .map(|(level, entry)| (VarIndex(level), entry))
    }

    pub fn len(&self) -> EnvLen { EnvLen(self.entries.len()) }

    pub fn get_by_index(&self, index: VarIndex) -> Option<&T> {
        self.get_by_level(self.len().index_to_level(index)?)
    }

    pub fn get_by_level(&self, level: VarLevel) -> Option<&T> { self.entries.get(level.0) }
}

impl<T: Clone> Extend<T> for SharedEnv<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        Rc::make_mut(&mut self.entries).extend(iter)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalEnv {
    pub names: UniqueEnv<Option<RcStr>>,
    pub types: UniqueEnv<Rc<Value>>,
    pub values: SharedEnv<Rc<Value>>,
}

#[debug_invariant(self.names.len() == self.types.len())]
#[debug_invariant(self.types.len() == self.values.len())]
impl LocalEnv {
    pub fn new() -> Self {
        Self {
            names: UniqueEnv::new(),
            types: UniqueEnv::new(),
            values: SharedEnv::new(),
        }
    }

    pub fn lookup(&self, name: &str) -> Option<(VarIndex, Rc<Value>)> {
        let (idx, _) = self.names.iter_by_index().find(|(_, n)| match n {
            Some(n) => n.as_ref() == name,
            None => false,
        })?;
        let ty = self.types.get_by_index(idx)?;
        Some((idx, ty.clone()))
    }

    pub fn push_param(&mut self, name: Option<RcStr>, ty: Rc<Value>) -> Rc<Value> {
        let value = Rc::new(Value::local(self.values.len().to_level()));
        self.names.push(name);
        self.types.push(ty);
        self.values.push(value.clone());
        value
    }

    pub fn pop(&mut self) {
        self.names.pop();
        self.types.pop();
        self.values.pop();
    }

    pub fn len(&self) -> EnvLen { self.names.len() }

    pub fn truncate(&mut self, len: EnvLen) {
        self.names.truncate(len);
        self.types.truncate(len);
        self.values.truncate(len);
    }
}
