use std::rc::Rc;

use contracts::debug_ensures;

use super::env::{UniqueEnv, VarLevel};
use super::{Decl, EntryInfo, Expr, LetDecl, Module, Pat, VarName};
use crate::{surface, RcStr};

pub struct UnelabCtx<'env> {
    item_names: &'env mut UniqueEnv<RcStr>,
    local_names: &'env mut UniqueEnv<VarName>,
}

impl<'env> UnelabCtx<'env> {
    pub fn new(
        item_names: &'env mut UniqueEnv<RcStr>,
        local_names: &'env mut UniqueEnv<VarName>,
    ) -> Self {
        Self {
            item_names,
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
    #[allow(clippy::option_if_let_else)]
    pub fn unelab_expr(&mut self, expr: &Expr) -> surface::Expr<()> {
        match expr {
            Expr::Error => surface::Expr::Error(()),
            Expr::Type => surface::Expr::Name((), "Type".into()),
            Expr::BoolType => surface::Expr::Name((), "Bool".into()),
            Expr::Bool(b) => surface::Expr::Bool((), *b),
            Expr::Local(index) => {
                let name = match self.local_names.get_by_index(*index) {
                    Some(VarName::User(name)) => name.clone(),
                    Some(VarName::Fresh) => todo!("gensym"),
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
            Expr::Meta(_) => surface::Expr::Placeholder(()),
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
                        let pat = self.unelab_arg(name, ty);
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
                        let pat = self.unelab_arg(name, ty);
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
                        let pat = self.unelab_pat(pat);
                        let expr = self.unelab_expr(expr);
                        self.local_names.truncate(initial_len);
                        (pat, expr)
                    })
                    .collect();
                surface::Expr::Match((), Rc::new(scrut), arms)
            }
            Expr::Let(name, init, body) => {
                let pat = match name {
                    VarName::User(name) => surface::Pat::Name((), name.clone()),
                    VarName::Fresh => todo!("gensym"),
                };
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

    fn unelab_arg(&mut self, name: &VarName, ty: &Expr) -> surface::Pat<()> {
        let pat = match name {
            VarName::User(name) => surface::Pat::Name((), name.clone()),
            VarName::Fresh => todo!("gensym"),
        };
        let ty = self.unelab_expr(ty);
        surface::Pat::Ann((), Rc::new(pat), Rc::new(ty))
    }

    fn unelab_pat(&mut self, pat: &Pat) -> surface::Pat<()> {
        match pat {
            Pat::Error => surface::Pat::Error(()),
            Pat::Name(name) => {
                let name = name.clone();
                self.local_names.push(name.clone());
                match name {
                    VarName::User(name) => surface::Pat::Name((), name),
                    VarName::Fresh => todo!("gensym"),
                }
            }
            Pat::Bool(b) => surface::Pat::Bool((), *b),
        }
    }
}
