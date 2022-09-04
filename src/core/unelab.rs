use std::rc::Rc;

use contracts::debug_ensures;

use super::env::{UniqueEnv, VarLevel};
use super::{Decl, EntryInfo, Expr, LetDecl, Module, Pat, VarName};
use crate::{surface, RcStr};

pub struct UnelabCtx<'env> {
    item_names: &'env UniqueEnv<RcStr>,
    meta_names: &'env UniqueEnv<VarName>,
    local_names: &'env mut UniqueEnv<VarName>,
}

impl<'env> UnelabCtx<'env> {
    pub fn new(
        item_names: &'env UniqueEnv<RcStr>,
        meta_names: &'env UniqueEnv<VarName>,
        local_names: &'env mut UniqueEnv<VarName>,
    ) -> Self {
        Self {
            item_names,
            meta_names,
            local_names,
        }
    }

    #[debug_ensures(self.local_names.len() == old(self.local_names.len()))]
    pub fn unelab_module(&mut self, module: &Module) -> surface::Module<()> {
        let Module { decls } = module;
        let decls = decls.iter().map(|decl| self.unelab_decl(decl)).collect();
        surface::Module { decls }
    }

    fn unelab_decl(&mut self, decl: &Decl) -> surface::Decl<()> {
        match decl {
            Decl::Error => surface::Decl::Error(()),
            Decl::Let(decl) => surface::Decl::Let((), self.unelab_let_decl(decl)),
        }
    }

    fn unelab_let_decl(&mut self, decl: &LetDecl) -> surface::LetDecl<()> {
        let LetDecl { name, ty, expr } = decl;
        let ty = self.unelab_expr(ty);
        let expr = self.unelab_expr(expr);
        surface::LetDecl {
            name: ((), name.clone()),
            ty: Some(Rc::new(ty)),
            expr: Rc::new(expr),
        }
    }

    #[debug_ensures(self.local_names.len() == old(self.local_names.len()))]
    pub fn unelab_expr(&mut self, expr: &Expr) -> surface::Expr<()> {
        match expr {
            Expr::Error => surface::Expr::Error(()),
            Expr::Type => surface::Expr::Name((), "Type".into()),
            Expr::BoolType => surface::Expr::Name((), "Bool".into()),
            Expr::Bool(b) => surface::Expr::Bool((), *b),
            Expr::Local(index) => {
                let name = match self.local_names.get_by_index(*index) {
                    Some(VarName::User(name)) => name.clone(),
                    Some(VarName::Fresh(count)) => self.gen_name(*count),
                    Some(VarName::Underscore) => {
                        unreachable!("Underscore cannot not be referenced by a local variable")
                    }
                    _ => unreachable!("Unbound local variable: {index:?}"),
                };
                surface::Expr::Name((), name)
            }
            Expr::Item(level) => {
                let name = match self.item_names.get_by_level(*level) {
                    Some(name) => name.clone(),
                    None => unreachable!("Unbound item variable: {level:?}"),
                };
                surface::Expr::Name((), name)
            }
            Expr::Meta(level) => {
                let name = match self.meta_names.get_by_level(*level) {
                    Some(VarName::User(name)) => name.clone(),
                    Some(VarName::Fresh(count)) => self.gen_name(*count),
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
                        EntryInfo::Def => {}
                        EntryInfo::Param => {
                            let var =
                                (self.local_names.len().level_to_index(VarLevel(var))).unwrap();
                            let arg = self.unelab_expr(&Expr::Local(var));
                            head = surface::Expr::FunCall((), Rc::new(head), Rc::from(vec![arg]))
                        }
                    }
                }
                head
            }
            Expr::FunType(names, args, ret) => {
                let initial_len = self.local_names.len();
                let pats = names
                    .iter()
                    .zip(args.iter())
                    .map(|(name, ty)| {
                        let pat = self.unelab_simple_pat(name, ty);
                        self.local_names.push(name.clone());
                        pat
                    })
                    .collect();
                let ret = self.unelab_expr(ret);
                self.local_names.truncate(initial_len);
                surface::Expr::FunType((), pats, Rc::new(ret))
            }
            Expr::FunExpr(names, args, body) => {
                let initial_len = self.local_names.len();
                let pats = names
                    .iter()
                    .zip(args.iter())
                    .map(|(name, ty)| {
                        let pat = self.unelab_simple_pat(name, ty);
                        self.local_names.push(name.clone());
                        pat
                    })
                    .collect();
                let body = self.unelab_expr(body);
                self.local_names.truncate(initial_len);
                surface::Expr::FunExpr((), pats, Rc::new(body))
            }
            Expr::FunCall(fun, args) => {
                let fun = self.unelab_expr(fun);
                let args = args.iter().map(|arg| self.unelab_expr(arg)).collect();
                surface::Expr::FunCall((), Rc::new(fun), args)
            }
            Expr::Match(scrut, arms) => {
                let scrut = self.unelab_expr(scrut);
                let arms = arms
                    .iter()
                    .map(|(pat, expr)| {
                        let initial_len = self.local_names.len();
                        let pat = self.unelab_match_pat(pat);
                        let expr = self.unelab_expr(expr);
                        self.local_names.truncate(initial_len);
                        (pat, expr)
                    })
                    .collect();
                surface::Expr::Match((), Rc::new(scrut), arms)
            }
            Expr::Let(name, ty, init, body) => {
                let pat = self.unelab_simple_pat(name, ty);

                let init = self.unelab_expr(init);
                self.local_names.push(name.clone());
                let body = self.unelab_expr(body);
                self.local_names.pop();
                surface::Expr::Let((), Rc::new(pat), Rc::new(init), Rc::new(body))
            }
            Expr::Ann(expr, ty) => {
                let expr = self.unelab_expr(expr);
                let ty = self.unelab_expr(ty);
                surface::Expr::Ann((), Rc::new(expr), Rc::new(ty))
            }
        }
    }

    fn unelab_simple_pat(&mut self, name: &VarName, ty: &Expr) -> surface::SimplePat<()> {
        let name = match name {
            VarName::User(name) => Some(name.clone()),
            VarName::Underscore => None,
            VarName::Fresh(count) => Some(self.gen_name(*count)),
        };
        let ty = self.unelab_expr(ty);
        surface::SimplePat {
            name: ((), name),
            ty: Some(Rc::new(ty)),
        }
    }

    fn unelab_var_pat(&mut self, name: &VarName) -> surface::Pat<()> {
        match name {
            VarName::User(name) => surface::Pat::Name((), name.clone()),
            VarName::Fresh(count) => surface::Pat::Name((), self.gen_name(*count)),
            VarName::Underscore => surface::Pat::Wildcard(()),
        }
    }

    fn unelab_match_pat(&mut self, pat: &Pat) -> surface::Pat<()> {
        match pat {
            Pat::Error => surface::Pat::Error(()),
            Pat::Bool(b) => surface::Pat::Bool((), *b),
            Pat::Name(name) => {
                self.local_names.push(name.clone());
                self.unelab_var_pat(name)
            }
        }
    }

    fn gen_name(&mut self, count: u32) -> RcStr {
        // FIXME: something nicer?
        Rc::from(format!("${count}"))
    }
}
