use std::rc::Rc;

use super::env::UniqueEnv;
use super::Expr;
use crate::{surface, RcStr};

pub struct UnelCtx<'env> {
    local_names: &'env mut UniqueEnv<Option<RcStr>>,
}

impl<'env> UnelCtx<'env> {
    pub fn new(local_names: &'env mut UniqueEnv<Option<RcStr>>) -> Self { Self { local_names } }

    pub fn unelab_expr(&mut self, expr: &Expr) -> surface::Expr<()> {
        match expr {
            Expr::Error => surface::Expr::Error(()),
            Expr::Type => surface::Expr::Name((), "Type".into()),
            Expr::BoolType => surface::Expr::Name((), "Bool".into()),
            Expr::Bool(b) => surface::Expr::Bool((), *b),
            Expr::Local(index) => {
                let name = match self.local_names.get_by_index(*index) {
                    Some(Some(name)) => name,
                    _ => unreachable!(),
                };
                surface::Expr::Name((), name.clone())
            }
            Expr::FunType(names, args, ret) => {
                let pats = names
                    .iter()
                    .zip(args.iter())
                    .map(|(name, ty)| self.unelab_arg(name, ty))
                    .collect();
                let ret = self.unelab_expr(ret);
                surface::Expr::FunType((), pats, Rc::new(ret))
            }
            Expr::FunExpr(names, args, body) => {
                let pats = names
                    .iter()
                    .zip(args.iter())
                    .map(|(name, ty)| self.unelab_arg(name, ty))
                    .collect();
                let ret = self.unelab_expr(body);
                surface::Expr::FunExpr((), pats, Rc::new(ret))
            }
            Expr::FunCall(fun, args) => {
                let fun = self.unelab_expr(fun);
                let args = args.iter().map(|arg| self.unelab_expr(arg)).collect();
                surface::Expr::FunCall((), Rc::new(fun), args)
            }
            Expr::Let(..) => todo!(),
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
