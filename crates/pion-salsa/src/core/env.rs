use std::sync::Arc;

use contracts::debug_invariant;

use super::syntax::{Value, VarName};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct VarIndex(usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct VarLevel(usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct EnvLen(pub usize);

impl EnvLen {
    pub fn index_to_level(&self, index: VarIndex) -> Option<VarLevel> {
        Some(VarLevel(self.0.checked_sub(index.0)?.checked_sub(1)?))
    }

    pub fn level_to_index(&self, level: VarLevel) -> Option<VarIndex> {
        Some(VarIndex(self.0.checked_sub(level.0)?.checked_sub(1)?))
    }

    pub fn to_level(self) -> VarLevel { VarLevel(self.0) }

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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UniqueEnv<T> {
    entries: Vec<T>,
}

pub trait ToLevel {
    fn to_level(self, len: EnvLen) -> Option<VarLevel>;
}

impl ToLevel for VarLevel {
    fn to_level(self, _: EnvLen) -> Option<VarLevel> { Some(self) }
}

impl ToLevel for VarIndex {
    fn to_level(self, len: EnvLen) -> Option<VarLevel> { len.index_to_level(self) }
}

impl<T> UniqueEnv<T> {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    pub fn len(&self) -> EnvLen { EnvLen(self.entries.len()) }

    pub fn pop(&mut self) { self.entries.pop(); }

    pub fn push(&mut self, elem: T) { self.entries.push(elem); }

    pub fn truncate(&mut self, len: EnvLen) { self.entries.truncate(len.0); }

    pub fn get<I>(&self, index: I) -> Option<&T>
    where
        I: ToLevel,
    {
        self.entries.get(index.to_level(self.len())?.0)
    }

    pub fn set<I>(&mut self, index: I, elem: T)
    where
        I: ToLevel,
    {
        let index = index.to_level(self.len()).unwrap().0;
        self.entries[index] = elem;
    }

    pub fn resize_with(&mut self, len: EnvLen, f: impl FnMut() -> T) {
        self.entries.resize_with(len.0, f);
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = &T> { self.entries.iter() }
}

impl<T> Default for UniqueEnv<T> {
    fn default() -> Self { Self::new() }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SharedEnv<T> {
    entries: Arc<Vec<T>>,
}

impl<T> SharedEnv<T> {
    pub fn new() -> Self {
        Self {
            entries: Arc::new(Vec::new()),
        }
    }

    pub fn len(&self) -> EnvLen { EnvLen(self.entries.len()) }

    pub fn pop(&mut self)
    where
        T: Clone,
    {
        Arc::make_mut(&mut self.entries).pop();
    }

    pub fn push(&mut self, elem: T)
    where
        T: Clone,
    {
        Arc::make_mut(&mut self.entries).push(elem);
    }

    pub fn truncate(&mut self, len: EnvLen)
    where
        T: Clone,
    {
        Arc::make_mut(&mut self.entries).truncate(len.0);
    }

    pub fn get<I>(&mut self, index: I) -> Option<&T>
    where
        I: ToLevel,
    {
        self.entries.get(index.to_level(self.len())?.0)
    }

    pub fn extend<I>(&mut self, iter: I)
    where
        T: Clone,
        I: IntoIterator<Item = T>,
    {
        Arc::make_mut(&mut self.entries).extend(iter)
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = &T> { self.entries.iter() }
}

impl<T> Default for SharedEnv<T> {
    fn default() -> Self { Self::new() }
}

pub struct LocalEnv {
    pub names: UniqueEnv<VarName>,
    pub sources: SharedEnv<LocalSource>,
    pub types: UniqueEnv<Arc<Value>>,
    pub values: SharedEnv<Arc<Value>>,
}

#[debug_invariant(self.names.len() == self.sources.len())]
#[debug_invariant(self.names.len() == self.types.len())]
#[debug_invariant(self.names.len() == self.values.len())]
impl LocalEnv {
    pub fn new() -> Self {
        Self {
            names: UniqueEnv::new(),
            sources: SharedEnv::new(),
            types: UniqueEnv::new(),
            values: SharedEnv::new(),
        }
    }

    pub fn len(&self) -> EnvLen { self.names.len() }

    pub fn pop(&mut self) {
        self.names.pop();
        self.sources.pop();
        self.values.pop();
        self.types.pop();
    }

    pub fn push_def(&mut self, name: VarName, ty: Arc<Value>, value: Arc<Value>) {
        self.names.push(name);
        self.sources.push(LocalSource::Def);
        self.types.push(ty);
        self.values.push(value);
    }

    pub fn push_param(&mut self, name: VarName, ty: Arc<Value>) -> Arc<Value> {
        let value = Arc::new(Value::local(self.values.len().to_level()));
        self.names.push(name);
        self.sources.push(LocalSource::Param);
        self.types.push(ty);
        self.values.push(value.clone());
        value
    }

    pub fn truncate(&mut self, len: EnvLen) {
        self.names.truncate(len);
        self.sources.truncate(len);
        self.types.truncate(len);
        self.values.truncate(len);
    }

    pub fn lookup(&self, name: &str) -> Option<(VarIndex, Arc<Value>)> {
        let index = self
            .names
            .entries
            .iter()
            .rev()
            .enumerate()
            .find(|(_, n)| match n {
                VarName::User(n) => n == name,
                _ => false,
            })
            .map(|(index, _)| VarIndex(index))?;
        let ty = self.types.get(index)?;
        Some((index, ty.clone()))
    }
}

pub struct MetaEnv {
    pub names: UniqueEnv<VarName>,
    pub sources: UniqueEnv<MetaSource>,
    pub types: UniqueEnv<Arc<Value>>,
    pub values: UniqueEnv<Option<Arc<Value>>>,
}

#[debug_invariant(self.names.len() == self.sources.len())]
#[debug_invariant(self.names.len() == self.types.len())]
#[debug_invariant(self.names.len() == self.values.len())]
impl MetaEnv {
    pub fn new() -> Self {
        Self {
            names: UniqueEnv::new(),
            sources: UniqueEnv::new(),
            types: UniqueEnv::new(),
            values: UniqueEnv::new(),
        }
    }

    pub fn push(&mut self, name: VarName, source: MetaSource, ty: Arc<Value>) -> VarLevel {
        let var = self.values.len().to_level();
        self.names.push(name);
        self.sources.push(source);
        self.types.push(ty);
        self.values.push(None);
        var
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LocalSource {
    Param,
    Def,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MetaSource {
    Error,
    PatType,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct NameSource(u32);

impl NameSource {
    pub fn new(counter: u32) -> Self { Self(counter) }

    pub fn fresh(&mut self) -> VarName {
        let next = VarName::Synth(self.0);
        self.0 += 1;
        next
    }

    pub fn current(&self) -> VarName { VarName::Synth(self.0) }

    pub fn truncate(&mut self, new_count: u32) { self.0 = new_count; }

    pub fn reset(&mut self) { self.0 = 0; }
}
