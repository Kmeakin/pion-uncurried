use pretty::{docs, Doc, DocAllocator, RcAllocator, RcDoc};

use super::syntax::*;

pub struct PrettyCtx {
    alloc: RcAllocator,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Prec {
    Let,
    Fun,
    Call,
    Atom,
}

impl Prec {
    pub const MAX: Self = Self::Let;
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

    pub fn pretty_module<Span>(&'a self, module: &Module<Span>) -> DocBuilder<'a> {
        let items = module.items.iter().map(|decl| self.pretty_item(decl));
        let sep = docs!(self, self.line(), self.line());
        self.intersperse(items, sep)
    }

    pub fn pretty_item<Span>(&'a self, item: &Item<Span>) -> DocBuilder<'a> {
        match item {
            Item::Let(let_def) => self.pretty_let_def(let_def),
            Item::Enum(enum_def) => self.pretty_enum_def(enum_def),
        }
    }

    pub fn pretty_let_def<Span>(&'a self, let_def: &LetDef<Span>) -> DocBuilder<'a> {
        let LetDef { name, ty, body } = let_def;
        let ty = self.pretty_type_annotation(ty);
        let body = self.pretty_expr(body);
        docs!(
            self,
            "let",
            self.space(),
            name.clone(),
            ty,
            self.space(),
            "=",
            self.space(),
            body,
            ";"
        )
    }

    pub fn pretty_enum_def<Span>(&'a self, enum_def: &EnumDef<Span>) -> DocBuilder<'a> {
        let EnumDef {
            name,
            args,
            ret_type: ty,
            variants,
        } = enum_def;
        let pats = args.iter().map(|pat| self.pretty_ann_pat(pat));
        let sep = docs!(self, ",", self.space());
        let pats = self.intersperse(pats, sep);
        let pats = if args.is_empty() {
            pats
        } else {
            docs!(self, "(", pats, ")")
        };
        let ty = self.pretty_type_annotation(ty);
        let variants_empty = variants.is_empty();
        let variants = variants.iter().map(|variant| {
            let variant = self.pretty_enum_variant(variant);
            docs!(self, self.hardline(), variant, ",")
        });
        docs!(
            self,
            "enum",
            self.space(),
            name.clone(),
            pats,
            ty,
            self.space(),
            "{",
            self.concat(variants).nest(INDENT),
            (!variants_empty).then_some(self.hardline()),
            "}"
        )
    }

    pub fn pretty_enum_variant<Span>(&'a self, enum_variant: &EnumVariant<Span>) -> DocBuilder<'a> {
        let EnumVariant {
            name,
            args,
            ret_type: ty,
        } = enum_variant;
        let pats = args.iter().map(|pat| self.pretty_ann_pat(pat));
        let sep = docs!(self, ",", self.space());
        let pats = self.intersperse(pats, sep);
        let pats = if args.is_empty() {
            pats
        } else {
            docs!(self, "(", pats, ")")
        };
        let ty = self.pretty_type_annotation(ty);
        docs!(self, name.clone(), pats, ty)
    }

    pub fn pretty_expr<Span>(&'a self, expr: &Expr<Span>) -> DocBuilder<'a> {
        self.pretty_expr_prec(Prec::MAX, expr)
    }

    pub fn pretty_expr_prec<Span>(&'a self, prec: Prec, expr: &Expr<Span>) -> DocBuilder<'a> {
        match expr {
            Expr::Error(_) => self.text("#error"),
            Expr::Hole(_, Hole::Underscore) => self.text("?_"),
            Expr::Hole(_, Hole::Name(name)) => self.text(format!("?{}", name)),
            Expr::Name(_, name) => self.text(name.to_string()),
            Expr::Lit(_, lit) => self.pretty_lit(lit),
            Expr::FunType(_, pats, ret) => {
                let pats = pats.iter().map(|pat| self.pretty_ann_pat(pat));
                let sep = docs!(self, ",", self.space());
                let pats = self.intersperse(pats, sep);
                let ret = self.pretty_expr_prec(Prec::Fun, ret);
                self.parens(
                    prec > Prec::Fun,
                    docs!(
                        self,
                        "fn",
                        "(",
                        pats,
                        ")",
                        self.space(),
                        "->",
                        self.space(),
                        ret
                    ),
                )
            }
            Expr::FunExpr(_, args, body) => {
                let args = args.iter().map(|pat| self.pretty_ann_pat(pat));
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
                let pat = self.pretty_ann_pat(pat);
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
                let is_empty = arms.is_empty();
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
                    "}"
                )
                .group()
            }
        }
    }

    pub fn pretty_pat<Span>(&self, pat: &Pat<Span>) -> DocBuilder {
        self.pretty_pat_prec(Prec::MAX, pat)
    }

    pub fn pretty_pat_prec<Span>(&self, _prec: Prec, pat: &Pat<Span>) -> DocBuilder {
        match pat {
            Pat::Error(_) => self.text("#error"),
            Pat::Wildcard(_) => self.text("_"),
            Pat::Name(_, name) => self.text(name.to_string()),
            Pat::Lit(_, lit) => self.pretty_lit(lit),
        }
    }

    pub fn pretty_ann_pat<Span>(&self, pat: &AnnPat<Span>) -> DocBuilder {
        let AnnPat { pat, ty } = pat;
        let pat = self.pretty_pat(pat);
        let ty = self.pretty_type_annotation(ty);
        docs!(self, pat, ty)
    }

    fn pretty_type_annotation<Span>(&'a self, ty: &Option<Expr<Span>>) -> DocBuilder {
        let ty = ty
            .as_ref()
            .map(|ty| docs!(self, ":", self.space(), self.pretty_expr(ty)));
        docs!(self, ty)
    }

    pub fn pretty_lit<Span>(&'a self, lit: &Lit<Span>) -> DocBuilder<'a> {
        match lit {
            Lit::Bool(_, true) => self.text("true"),
            Lit::Bool(_, false) => self.text("false"),
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
