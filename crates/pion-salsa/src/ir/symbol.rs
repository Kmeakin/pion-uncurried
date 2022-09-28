/// An interned string
#[salsa::interned]
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Symbol {
    #[return_ref]
    pub contents: String,
}

impl Symbol {
    pub fn intern(db: &dyn crate::Db, string: impl ToString) -> Self {
        Self::new(db, string.to_string())
    }

    pub fn as_str(self, db: &dyn crate::Db) -> &str { self.contents(db) }
}
