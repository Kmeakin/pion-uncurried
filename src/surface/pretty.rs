use pretty::{docs, Doc, DocAllocator, RcAllocator, RcDoc};

use super::{Expr, Pat, SimplePat};

pub struct PrettyCtx {
    alloc: RcAllocator,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Prec {
    Ann,
    Let,
    Fun,
    Call,
    Atom,
}

impl Prec {
    pub const MAX: Self = Self::Ann;
    pub const MIN: Self = Self::Atom;
}

const INDENT: isize = 4;

pub type DocBuilder<'a> = pretty::DocBuilder<'a, PrettyCtx, ()>;

impl<'a> PrettyCtx {
    pub fn new() -> Self { Self { alloc: RcAllocator } }

    fn parens(&'a self, wrap: bool, doc: DocBuilder<'a>) -> DocBuilder<'a> {
        if wrap {
            docs!(self, "(", doc, ")")
        } else {
            doc
        }
    }

    fn bool(&'a self, b: bool) -> DocBuilder<'a> { self.text(if b { "true" } else { "false" }) }

    pub fn pretty_module<Range>(&'a self, module: &super::Module<Range>) -> DocBuilder<'a> {
        let decls = module.decls.iter().map(|decl| self.pretty_decl(decl));
        let sep = docs!(self, self.line(), self.line());
        self.intersperse(decls, sep)
    }

    fn pretty_decl<Range>(&'a self, decl: &super::Decl<Range>) -> DocBuilder<'a> {
        match decl {
            super::Decl::Error(_) => self.text("#error"),
            super::Decl::Let(_, decl) => self.pretty_let_decl(decl),
        }
    }

    fn pretty_let_decl<Range>(&'a self, decl: &super::LetDecl<Range>) -> DocBuilder<'a> {
        let super::LetDecl { name, ty, expr } = decl;
        let name = name.1.as_ref().map_or("_", |name| name.as_ref());
        let ty = ty
            .as_ref()
            .map(|ty| docs!(self, ":", self.space(), self.pretty_expr(ty), self.space()));
        let expr = self.pretty_expr(expr);
        docs!(
            self,
            "let",
            self.space(),
            self.text(name.to_owned()),
            self.space(),
            ty,
            "=",
            self.space(),
            expr,
            ";"
        )
    }

    pub fn pretty_expr<Range>(&'a self, expr: &Expr<Range>) -> DocBuilder<'a> {
        self.pretty_expr_prec(Prec::MAX, expr)
    }

    pub fn pretty_expr_prec<Range>(&'a self, prec: Prec, expr: &Expr<Range>) -> DocBuilder<'a> {
        match expr {
            Expr::Error(_) => self.text("#error"),
            Expr::Placeholder(_) => self.text("_"),
            Expr::Name(_, name) => self.text(name.to_string()),
            Expr::Bool(_, b) => self.bool(*b),
            Expr::FunType(_, args, ret) => {
                let args = args.iter().map(|pat| self.pretty_simple_pat(pat));
                let sep = docs!(self, ",", self.space());
                let args = self.intersperse(args, sep);
                let ret = self.pretty_expr_prec(Prec::Fun, ret);
                self.parens(
                    prec > Prec::Fun,
                    docs!(
                        self,
                        "fn",
                        "(",
                        args,
                        ")",
                        self.space(),
                        "->",
                        self.space(),
                        ret
                    ),
                )
            }
            Expr::FunExpr(_, args, body) => {
                let args = args.iter().map(|pat| self.pretty_simple_pat(pat));
                let sep = docs!(self, ",", self.space());
                let args = self.intersperse(args, sep);
                let body = self.pretty_expr_prec(Prec::Fun, body);
                self.parens(
                    prec > Prec::Fun,
                    docs!(
                        self,
                        "fn",
                        "(",
                        args,
                        ")",
                        self.space(),
                        "=>",
                        self.space(),
                        body
                    ),
                )
            }
            Expr::FunCall(_, fun, args) => {
                let fun = self.pretty_expr_prec(Prec::Call, fun);
                let args = args.iter().map(|arg| self.pretty_expr_prec(Prec::MAX, arg));
                let sep = docs!(self, ",", self.space());
                let args = self.intersperse(args, sep);
                self.parens(prec > Prec::Call, docs!(self, fun, "(", args, ")"))
            }
            Expr::Let(_, pat, init, body) => {
                let pat = self.pretty_simple_pat(pat);
                let init = self.pretty_expr_prec(Prec::MAX, init);
                let body = self.pretty_expr_prec(Prec::Let, body);
                self.parens(
                    prec > Prec::Let,
                    docs!(
                        self,
                        "let",
                        self.space(),
                        pat,
                        self.space(),
                        "=",
                        self.space(),
                        init,
                        self.space(),
                        "in",
                        self.space(),
                        body
                    ),
                )
            }
            Expr::Match(_, scrut, arms) => {
                let scrut = self.pretty_expr(scrut);
                let is_empty = arms.len() == 0;
                let arms = arms.iter().map(|(pat, expr)| {
                    let pat = self.pretty_pat(pat);
                    let expr = self.pretty_expr(expr);
                    docs!(
                        self,
                        self.hardline(),
                        pat,
                        self.space(),
                        "=>",
                        self.space(),
                        expr,
                        ",",
                    )
                });
                docs!(
                    self,
                    "match",
                    self.space(),
                    scrut,
                    self.space(),
                    "{",
                    self.concat(arms).nest(INDENT),
                    (!is_empty).then_some(self.hardline()),
                    docs!(self, "}")
                )
                .group()
            }
            Expr::Ann(_, expr, ty) => {
                let expr = self.pretty_expr_prec(Prec::Call, expr);
                let ty = self.pretty_expr_prec(Prec::Fun, ty);
                self.parens(
                    prec > Prec::Ann,
                    docs!(self, expr, self.space(), ":", self.space(), ty),
                )
            }
        }
    }

    fn pretty_simple_pat<Range>(&self, pat: &SimplePat<Range>) -> DocBuilder {
        let SimplePat { name, ty } = pat;
        let name = match name {
            Some(name) => self.text(name.to_string()),
            None => self.text("_"),
        };
        let ty = ty.as_ref().map(|ty| {
            let ty = self.pretty_expr_prec(Prec::Fun, ty);
            docs!(self, self.space(), ":", self.space(), ty)
        });
        docs!(self, name, ty)
    }

    fn pretty_pat<Range>(&self, pat: &Pat<Range>) -> DocBuilder {
        self.pretty_pat_prec(Prec::MAX, pat)
    }

    fn pretty_pat_prec<Range>(&self, prec: Prec, pat: &Pat<Range>) -> DocBuilder {
        match pat {
            Pat::Error(_) => self.text("#error"),
            Pat::Wildcard(_) => self.text("_"),
            Pat::Name(_, name) => self.text(name.to_string()),
            Pat::Bool(_, b) => self.bool(*b),
            Pat::Ann(_, pat, ty) => {
                let pat = self.pretty_pat_prec(Prec::Ann, pat);
                let ty = self.pretty_expr_prec(Prec::Fun, ty);
                self.parens(
                    prec > Prec::Ann,
                    docs!(self, pat, self.space(), ":", self.space(), ty),
                )
            }
        }
    }
}

impl<'a, A: 'a> DocAllocator<'a, A> for PrettyCtx {
    type Doc = RcDoc<'a, A>;

    fn alloc(&'a self, doc: Doc<'a, Self::Doc, A>) -> Self::Doc { self.alloc.alloc(doc) }

    fn alloc_column_fn(
        &'a self,
        f: impl Fn(usize) -> Self::Doc + 'a,
    ) -> <Self::Doc as pretty::DocPtr<'a, A>>::ColumnFn {
        self.alloc.alloc_column_fn(f)
    }

    fn alloc_width_fn(
        &'a self,
        f: impl Fn(isize) -> Self::Doc + 'a,
    ) -> <Self::Doc as pretty::DocPtr<'a, A>>::WidthFn {
        self.alloc.alloc_width_fn(f)
    }
}
