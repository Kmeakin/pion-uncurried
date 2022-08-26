use std::rc::Rc;

use contracts::debug_ensures;

use super::env::{UniqueEnv, VarLevel};
use super::{Decl, EntryInfo, Expr, LetDecl, Module};
use crate::{surface, RcStr};

pub struct UnelabCtx<'env> {
    local_names: &'env mut UniqueEnv<Option<RcStr>>,
}

impl<'env> UnelabCtx<'env> {
    pub fn new(local_names: &'env mut UniqueEnv<Option<RcStr>>) -> Self { Self { local_names } }

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
                    Some(Some(name)) => name.clone(),
                    Some(None) => Rc::from("_"),
                    _ => unreachable!("Unbound local variable: {index:?}"),
                };
                surface::Expr::Name((), name)
            }
            Expr::Meta(_) => surface::Expr::Placeholder(()),
            Expr::MetaInsertion(level, infos) => {
                let mut head = self.unelab_expr(&Expr::Meta(*level));
                let mut args = Vec::new();
                for (info, var) in infos.iter().zip(0..) {
                    match info {
                        EntryInfo::Def => {}
                        EntryInfo::Param(depth) => {
                            let var = self
                                .local_names
                                .len()
                                .level_to_index(VarLevel(var))
                                .unwrap();
                            let arg = self.unelab_expr(&Expr::Local(var));
                            args.push(arg);
                            if *depth == 0 {
                                head = surface::Expr::FunCall(
                                    (),
                                    Rc::new(head),
                                    Rc::from(args.clone()),
                                );
                                args.clear();
                            }
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
            Expr::Let(name, init, body) => {
                let pat = match name {
                    Some(name) => surface::Pat::Name((), name.clone()),
                    None => surface::Pat::Wildcard(()),
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

    fn unelab_arg(&mut self, name: &Option<RcStr>, ty: &Expr) -> surface::Pat<()> {
        let pat = match name {
            Some(name) => surface::Pat::Name((), name.clone()),
            None => surface::Pat::Wildcard(()),
        };
        let ty = self.unelab_expr(ty);
        surface::Pat::Ann((), Rc::new(pat), Rc::new(ty))
    }
}
