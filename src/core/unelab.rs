use std::sync::Arc;

use contracts::{debug_ensures, debug_requires};

use super::env::{LocalSource, NameSource, UniqueEnv, VarLevel};
use super::syntax::*;
use crate::surface;

pub struct UnelabCtx<'a> {
    local_names: &'a mut UniqueEnv<VarName>,
    meta_names: &'a UniqueEnv<VarName>,

    name_source: &'a mut NameSource,
    db: &'a dyn crate::Db,
}

impl<'a> UnelabCtx<'a> {
    pub fn new(
        local_names: &'a mut UniqueEnv<VarName>,
        meta_names: &'a UniqueEnv<VarName>,
        name_source: &'a mut NameSource,
        db: &'a dyn crate::Db,
    ) -> Self {
        Self {
            local_names,
            meta_names,
            name_source,
            db,
        }
    }

    #[debug_requires(expr.is_closed(self.local_names.len(), self.meta_names.len()))]
    #[debug_ensures(self.local_names.len() == old(self.local_names.len()))]
    pub fn unelab_expr(&mut self, expr: &Expr) -> surface::Expr<()> {
        match expr {
            Expr::Error => surface::Expr::Error(()),
            Expr::Type => surface::Expr::Name((), "Type".into()),
            Expr::BoolType => surface::Expr::Name((), "Bool".into()),
            Expr::Lit(lit) => surface::Expr::Lit((), self.unelab_lit(lit)),
            Expr::Local(index) => {
                let name = match self.local_names.get(*index) {
                    Some(VarName::User(name)) => name.contents(self.db).clone(),
                    Some(VarName::Synth(count)) => self.gen_name(*count),
                    Some(VarName::Underscore) => {
                        unreachable!("Underscore cannot not be referenced by a local variable")
                    }
                    _ => unreachable!("Unbound local variable: {index:?}"),
                };
                surface::Expr::Name((), name)
            }
            Expr::LetDef(def) => {
                let name = def.name(self.db).contents(self.db).to_owned();
                surface::Expr::Name((), name)
            }
            Expr::EnumDef(def) => {
                let name = def.name(self.db).contents(self.db).to_owned();
                surface::Expr::Name((), name)
            }
            Expr::EnumVariant(def) => {
                let name = def.name(self.db).contents(self.db).to_owned();
                surface::Expr::Name((), name)
            }
            Expr::Meta(level) => {
                let name = match self.meta_names.get(*level) {
                    Some(VarName::User(name)) => name.contents(self.db).clone(),
                    Some(VarName::Synth(count)) => self.gen_name(*count),
                    Some(VarName::Underscore) => {
                        unreachable!("Underscore cannot not be referenced by a local variable")
                    }
                    None => unreachable!("Unbound meta variable: {level:?}"),
                };
                surface::Expr::Hole((), surface::Hole::Name(name))
            }
            Expr::MetaInsertion(level, infos) => {
                let mut head = self.unelab_expr(&Expr::Meta(*level));
                for (info, var) in infos.iter().zip(0..) {
                    match info {
                        LocalSource::Def => {}
                        LocalSource::Param => {
                            let var =
                                (self.local_names.len().level_to_index(VarLevel(var))).unwrap();
                            let arg = self.unelab_expr(&Expr::Local(var));
                            head = surface::Expr::FunCall((), Arc::new(head), vec![arg])
                        }
                    }
                }
                head
            }
            Expr::FunType(args, ret) => {
                let initial_len = self.local_names.len();
                let pats = args
                    .iter()
                    .map(|FunArg { pat, ty }| {
                        let pat_surface = self.unelab_pat(pat);
                        let type_surface = self.unelab_expr(ty);
                        self.subst_pat(pat);
                        surface::AnnPat {
                            pat: pat_surface,
                            ty: Some(type_surface),
                        }
                    })
                    .collect();
                let ret = self.unelab_expr(ret);
                self.local_names.truncate(initial_len);
                surface::Expr::FunType((), pats, Arc::new(ret))
            }
            Expr::FunExpr(args, ret) => {
                let initial_len = self.local_names.len();
                let pats = args
                    .iter()
                    .map(|FunArg { pat, ty }| {
                        let pat_surface = self.unelab_pat(pat);
                        let type_surface = self.unelab_expr(ty);
                        self.subst_pat(pat);
                        surface::AnnPat {
                            pat: pat_surface,
                            ty: Some(type_surface),
                        }
                    })
                    .collect();
                let ret = self.unelab_expr(ret);
                self.local_names.truncate(initial_len);
                surface::Expr::FunExpr((), pats, Arc::new(ret))
            }
            Expr::FunCall(fun, args) => {
                let fun = self.unelab_expr(fun);
                let args = args.iter().map(|arg| self.unelab_expr(arg)).collect();
                surface::Expr::FunCall((), Arc::new(fun), args)
            }
            Expr::Let(pat, ty, init, body) => {
                let pat_surface = self.unelab_pat(pat);
                let type_surface = self.unelab_expr(ty);
                let init = self.unelab_expr(init);
                let initial_len = self.local_names.len();
                let body = self.unelab_expr(body);
                self.local_names.truncate(initial_len);
                surface::Expr::Let(
                    (),
                    Arc::new(surface::AnnPat {
                        pat: pat_surface,
                        ty: Some(type_surface),
                    }),
                    Arc::new(init),
                    Arc::new(body),
                )
            }
            Expr::Match(scrut, arms) => {
                let scrut = self.unelab_expr(scrut);
                let arms = arms
                    .iter()
                    .map(|(pat, body)| {
                        let initial_len = self.local_names.len();
                        let pat_surface = self.unelab_pat(pat);
                        self.subst_pat(pat);
                        let body_surface = self.unelab_expr(body);
                        self.local_names.truncate(initial_len);
                        (pat_surface, body_surface)
                    })
                    .collect();
                surface::Expr::Match((), Arc::new(scrut), arms)
            }
        }
    }

    #[debug_ensures(self.local_names.len() == old(self.local_names.len()))]
    pub fn unelab_pat(&mut self, pat: &Pat) -> surface::Pat<()> {
        match pat {
            Pat::Error => surface::Pat::Error(()),
            Pat::Lit(lit) => surface::Pat::Lit((), self.unelab_lit(lit)),
            Pat::Name(name) => match name {
                VarName::User(name) => surface::Pat::Name((), name.contents(self.db).clone()),
                VarName::Synth(count) => surface::Pat::Name((), self.gen_name(*count)),
                VarName::Underscore => surface::Pat::Wildcard(()),
            },
            Pat::Variant(variant, pats) => {
                let name = variant.name(self.db).contents(self.db);
                let pats = pats.iter().map(|pat| self.unelab_pat(pat)).collect();
                surface::Pat::Variant((), name.clone(), pats)
            }
        }
    }

    #[debug_ensures(self.local_names.len() == old(self.local_names.len()))]
    pub fn unelab_lit(&mut self, lit: &Lit) -> surface::Lit<()> {
        match lit {
            Lit::Bool(b) => surface::Lit::Bool((), *b),
        }
    }

    fn subst_pat(&mut self, pat: &Pat) {
        match pat {
            Pat::Error => self.local_names.push(VarName::Underscore),
            Pat::Name(name) => self.local_names.push(*name),
            Pat::Lit(_) => self.local_names.push(self.name_source.fresh()),
            Pat::Variant(_, pats) => pats.iter().for_each(|pat| self.subst_pat(pat)),
        }
    }

    fn gen_name(&mut self, count: u32) -> String {
        // FIXME: something nicer?
        format!("${count}")
    }
}

pub fn unelab_module(db: &dyn crate::Db, module: &Module) -> surface::Module<()> {
    let Module { items } = module;
    let items = items.iter().map(|item| unelab_item(db, item)).collect();
    surface::Module { items }
}

pub fn unelab_item(db: &dyn crate::Db, item: &Item) -> surface::Item<()> {
    match item {
        Item::Let(let_def) => surface::Item::Let(unelab_let_def(db, let_def)),
        Item::Enum(enum_def) => surface::Item::Enum(unelab_enum_def(db, enum_def)),
    }
}

pub fn unelab_let_def(db: &dyn crate::Db, let_def: &LetDef) -> surface::LetDef<()> {
    let mut local_names = UniqueEnv::new();
    let meta_names = UniqueEnv::new();
    let mut name_source = NameSource::new(0);
    let mut ctx = UnelabCtx::new(&mut local_names, &meta_names, &mut name_source, db);

    let LetDef { name, body, ty, .. } = let_def;

    let name = name.contents(db).clone();
    let ty = ctx.unelab_expr(&ty.0);
    let body = ctx.unelab_expr(&body.0);
    surface::LetDef {
        name,
        ty: Some(ty),
        body,
    }
}

pub fn unelab_enum_def(db: &dyn crate::Db, enum_def: &EnumDef) -> surface::EnumDef<()> {
    let mut local_names = UniqueEnv::new();
    let meta_names = UniqueEnv::new();
    let mut name_source = NameSource::new(0);
    let mut ctx = UnelabCtx::new(&mut local_names, &meta_names, &mut name_source, db);

    let EnumDef {
        name,
        args,
        ret_type,
        variants,
    } = enum_def;

    let name = name.contents(db).clone();

    let args = args
        .iter()
        .map(|FunArg { pat, ty }| {
            let pat_surface = ctx.unelab_pat(pat);
            let type_surface = ctx.unelab_expr(ty);
            ctx.subst_pat(pat);
            surface::AnnPat {
                pat: pat_surface,
                ty: Some(type_surface),
            }
        })
        .collect();

    let ret_type = ctx.unelab_expr(&ret_type.0);

    let variants = variants
        .iter()
        .map(
            |EnumVariant {
                 name,
                 args,
                 ret_type,
             }| {
                let initial_len = ctx.local_names.len();
                let name = name.contents(db).to_owned();
                let args = args
                    .iter()
                    .map(|FunArg { pat, ty }| {
                        let pat_surface = ctx.unelab_pat(pat);
                        let type_surface = ctx.unelab_expr(ty);
                        ctx.subst_pat(pat);
                        surface::AnnPat {
                            pat: pat_surface,
                            ty: Some(type_surface),
                        }
                    })
                    .collect();
                let ret_type = ctx.unelab_expr(&ret_type.0);
                ctx.local_names.truncate(initial_len);
                surface::EnumVariant {
                    name,
                    args,
                    ret_type: Some(ret_type),
                }
            },
        )
        .collect();

    surface::EnumDef {
        name,
        args,
        ret_type: Some(ret_type),
        variants,
    }
}
