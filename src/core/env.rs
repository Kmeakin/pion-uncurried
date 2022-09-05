use std::collections::HashMap;
use std::rc::Rc;

use contracts::debug_invariant;

use super::{EntryInfo, EnumDecl, MetaSource, Value, VarName};
use crate::RcStr;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct VarIndex(pub usize);

impl VarIndex {
    pub fn succ(&self) -> Self { Self(self.0 + 1) }
    pub fn succ_by(&self, n: usize) -> Self { Self(self.0 + n) }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct VarLevel(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
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

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &T> { self.entries.iter() }

    pub fn len(&self) -> EnvLen { EnvLen(self.entries.len()) }

    pub fn get_by_index(&self, index: VarIndex) -> Option<&T> {
        self.get_by_level(self.len().index_to_level(index)?)
    }

    pub fn get_by_level(&self, level: VarLevel) -> Option<&T> { self.entries.get(level.0) }

    pub fn clear(&mut self) { self.entries.clear(); }

    pub fn resize(&mut self, new_len: EnvLen, value: T)
    where
        T: Clone,
    {
        self.entries.resize(new_len.0, value)
    }

    pub fn resize_with(&mut self, new_len: EnvLen, f: impl FnMut() -> T) {
        self.entries.resize_with(new_len.0, f)
    }

    pub fn set_by_level(&mut self, level: VarLevel, elem: T) { self.entries[level.0] = elem; }
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

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &T> { self.entries.iter() }

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
    pub names: UniqueEnv<VarName>,
    pub infos: SharedEnv<EntryInfo>,
    pub types: UniqueEnv<Rc<Value>>,
    pub values: SharedEnv<Rc<Value>>,
}

#[debug_invariant(self.names.len() == self.infos.len())]
#[debug_invariant(self.infos.len() == self.types.len())]
#[debug_invariant(self.types.len() == self.values.len())]
impl LocalEnv {
    pub fn new() -> Self {
        Self {
            names: UniqueEnv::new(),
            types: UniqueEnv::new(),
            infos: SharedEnv::new(),
            values: SharedEnv::new(),
        }
    }

    pub fn lookup(&self, name: &str) -> Option<(VarIndex, Rc<Value>)> {
        let (idx, _) = self
            .names
            .entries
            .iter()
            .rev()
            .enumerate()
            .map(|(level, entry)| (VarIndex(level), entry))
            .find(|(_, n)| match n {
                VarName::User(n) => n.as_ref() == name,
                _ => false,
            })?;
        let ty = self.types.get_by_index(idx)?;
        Some((idx, ty.clone()))
    }

    pub fn push_param(&mut self, name: VarName, ty: Rc<Value>) -> Rc<Value> {
        let value = Rc::new(Value::local(self.values.len().to_level()));
        self.names.push(name);
        self.types.push(ty);
        self.infos.push(EntryInfo::Param);
        self.values.push(value.clone());
        value
    }

    pub fn push_def(&mut self, name: VarName, value: Rc<Value>, ty: Rc<Value>) -> Rc<Value> {
        self.names.push(name);
        self.types.push(ty);
        self.infos.push(EntryInfo::Def);
        self.values.push(value.clone());
        value
    }

    pub fn pop(&mut self) {
        self.names.pop();
        self.types.pop();
        self.infos.pop();
        self.values.pop();
    }

    pub fn len(&self) -> EnvLen { self.names.len() }

    pub fn truncate(&mut self, len: EnvLen) {
        self.names.truncate(len);
        self.types.truncate(len);
        self.infos.truncate(len);
        self.values.truncate(len);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MetaEnv {
    pub names: UniqueEnv<VarName>,
    pub sources: UniqueEnv<MetaSource>,
    pub types: UniqueEnv<Rc<Value>>,
    pub values: UniqueEnv<Option<Rc<Value>>>,
}

#[debug_invariant(self.names.len() == self.sources.len())]
#[debug_invariant(self.sources.len() == self.types.len())]
#[debug_invariant(self.types.len() == self.values.len())]
impl MetaEnv {
    pub fn new() -> Self {
        Self {
            names: UniqueEnv::new(),
            sources: UniqueEnv::new(),
            types: UniqueEnv::new(),
            values: UniqueEnv::new(),
        }
    }

    pub fn push(&mut self, name: VarName, source: MetaSource, ty: Rc<Value>) -> VarLevel {
        let var = self.values.len().to_level();
        self.names.push(name);
        self.sources.push(source);
        self.types.push(ty);
        self.values.push(None);
        var
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemEnv {
    pub name_to_level: HashMap<RcStr, VarLevel>,
    pub level_to_name: UniqueEnv<RcStr>,
    pub types: UniqueEnv<Rc<Value>>,
    pub values: UniqueEnv<Rc<Value>>,
}

#[debug_invariant(self.name_to_level.len() == self.level_to_name.len().0)]
#[debug_invariant(self.level_to_name.len() == self.types.len())]
#[debug_invariant(self.types.len() == self.values.len())]
impl ItemEnv {
    pub fn new() -> Self {
        Self {
            name_to_level: HashMap::new(),
            level_to_name: UniqueEnv::new(),
            types: UniqueEnv::new(),
            values: UniqueEnv::new(),
        }
    }

    pub fn lookup(&self, name: &str) -> Option<(VarLevel, Rc<Value>)> {
        let level = self.name_to_level.get(name)?;
        let ty = self.types.get_by_level(*level)?;
        Some((*level, ty.clone()))
    }

    pub fn push(&mut self, name: RcStr, ty: Rc<Value>, value: Rc<Value>) {
        let level = VarLevel(self.name_to_level.len());
        self.name_to_level.insert(name.clone(), level);
        self.level_to_name.push(name);
        self.types.push(ty);
        self.values.push(value);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumEnv {
    pub names: UniqueEnv<RcStr>,
    pub decls: UniqueEnv<EnumDecl>,
    pub types: UniqueEnv<Rc<Value>>,
}

// #[debug_invariant(self.decls.len() == self.types.len())]
impl EnumEnv {
    pub fn new() -> Self {
        Self {
            names: UniqueEnv::new(),
            decls: UniqueEnv::new(),
            types: UniqueEnv::new(),
        }
    }

    pub fn lookup(&self, name: &str) -> Option<(VarLevel, &EnumDecl, &Rc<Value>)> {
        self.decls.iter().enumerate().find_map(|(level, decl)| {
            let (level, decl) = if decl.name.as_ref() == name {
                Some((VarLevel(level), decl))
            } else {
                None
            }?;
            let ty = self.types.get_by_level(level)?;
            Some((level, decl, ty))
        })
    }

    pub fn push(&mut self, decl: EnumDecl, ty: Rc<Value>) {
        self.names.push(decl.name.clone());
        self.decls.push(decl);
        self.types.push(ty);
    }

    pub fn len(&self) -> EnvLen { self.decls.len() }
}
