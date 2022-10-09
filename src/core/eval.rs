use std::sync::Arc;

use contracts::{debug_ensures, debug_requires};
use either::Either;
use either::Either::{Left, Right};

use super::env::{EnvLen, LocalSource, SharedEnv, UniqueEnv};
use super::quote::QuoteCtx;
use super::syntax::*;

pub struct EvalCtx<'env> {
    local_env: &'env mut SharedEnv<Arc<Value>>,
    meta_env: &'env UniqueEnv<Option<Arc<Value>>>,
    db: &'env dyn crate::Db,
}

impl<'env> EvalCtx<'env> {
    pub fn new(
        local_env: &'env mut SharedEnv<Arc<Value>>,
        meta_env: &'env UniqueEnv<Option<Arc<Value>>>,
        db: &'env dyn crate::Db,
    ) -> Self {
        Self {
            local_env,
            meta_env,
            db,
        }
    }

    fn elim_ctx(&self) -> ElimCtx { ElimCtx::new(self.meta_env, self.db) }

    fn quote_ctx(&self) -> QuoteCtx { QuoteCtx::new(self.local_env.len(), self.meta_env, self.db) }

    #[debug_requires(expr.is_closed(self.local_env.len(), self.meta_env.len()))]
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    pub fn normalize_expr(&mut self, expr: &Expr) -> (Expr, Arc<Value>) {
        let value = self.eval_expr(expr);
        let expr = self.quote_ctx().quote_value(&value);
        (expr, value)
    }

    #[debug_requires(expr.is_closed(self.local_env.len(), self.meta_env.len()))]
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    pub fn eval_expr(&mut self, expr: &Expr) -> Arc<Value> {
        match expr {
            Expr::Error => Arc::new(Value::Error),
            Expr::Type => Arc::new(Value::Type),
            Expr::BoolType => Arc::new(Value::BoolType),
            Expr::Lit(lit) => Arc::new(Value::Lit(lit.clone())),
            Expr::LetDef(ir) => super::elab::eval_let_def_expr(self.db, *ir),
            Expr::EnumDef(enum_def) => Arc::new(Value::enum_def(*enum_def)),
            Expr::EnumVariant(enum_variant) => Arc::new(Value::enum_variant(*enum_variant)),
            Expr::Local(index) => match self.local_env.get(*index) {
                Some(value) => value.clone(),
                None => unreachable!("Unbound local variable: {index:?}"),
            },
            Expr::Meta(level) => match self.meta_env.get(*level) {
                Some(Some(value)) => value.clone(),
                Some(None) => Arc::new(Value::meta(*level)),
                None => unreachable!("Unbound meta variable: {level:?}"),
            },
            Expr::MetaInsertion(level, sources) => {
                let mut head = self.eval_expr(&Expr::Meta(*level));
                self.fold_local_sources(head, sources.iter().copied())
            }
            Expr::FunType(args, ret) => Arc::new(Value::FunType(FunClosure::new(
                self.local_env.clone(),
                args.clone(),
                ret.clone(),
            ))),
            Expr::FunExpr(args, body) => Arc::new(Value::FunValue(FunClosure::new(
                self.local_env.clone(),
                args.clone(),
                body.clone(),
            ))),
            Expr::FunCall(fun, args) => {
                let fun = self.eval_expr(fun);
                let args = args.iter().map(|arg| self.eval_expr(arg)).collect();
                self.elim_ctx().do_fun_call(fun, args)
            }
            Expr::Let(_, _, init, body) => {
                let init_value = self.eval_expr(init);
                self.local_env.push(init_value);
                let body_value = self.eval_expr(body);
                self.local_env.pop();
                body_value
            }
            Expr::Match(scrut, arms) => {
                let scrut = self.eval_expr(scrut);
                let arms = MatchArms::new(self.local_env.clone(), arms.clone());
                self.elim_ctx().do_match(scrut, arms)
            }
        }
    }

    fn fold_local_sources(
        &mut self,
        mut head: Arc<Value>,
        sources: impl IntoIterator<Item = LocalSource>,
    ) -> Arc<Value> {
        for (source, value) in sources.into_iter().zip(self.local_env.iter()) {
            head = match source {
                LocalSource::Def => head,
                LocalSource::Param => self.elim_ctx().do_fun_call(head, vec![value.clone()]),
            };
        }
        head
    }

    #[debug_requires(expr.is_closed(self.local_env.len(), self.meta_env.len()))]
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    #[debug_ensures(ret.is_closed(self.local_env.len(), EnvLen(0)))]
    pub fn zonk_expr(&mut self, expr: &Expr) -> Expr {
        match expr {
            Expr::Error
            | Expr::Type
            | Expr::BoolType
            | Expr::Lit(_)
            | Expr::LetDef(_)
            | Expr::EnumDef(_)
            | Expr::EnumVariant(_)
            | Expr::Local(_) => expr.clone(),

            Expr::Meta(_) | Expr::FunCall(..) | Expr::Match(..) => match self.zonk_spine(expr) {
                Left(expr) => expr,
                Right(value) => self.quote_ctx().quote_value(&value),
            },

            Expr::MetaInsertion(var, sources) => match self.meta_env.get(*var) {
                Some(Some(value)) => {
                    let value = self.fold_local_sources(value.clone(), sources.iter().copied());
                    self.quote_ctx().quote_value(&value)
                }
                Some(None) => Expr::Error,
                None => unreachable!("Unbound meta variable {var:#?}"),
            },

            Expr::FunType(args, ret) => {
                let initial_len = self.local_env.len();
                let args = args
                    .iter()
                    .map(|FunArg { pat, ty }| {
                        let pat = pat.clone();
                        let ty = self.zonk_expr(ty);
                        self.subst_pat(&pat);
                        FunArg { pat, ty }
                    })
                    .collect();
                let ret = self.zonk_expr(ret);
                self.local_env.truncate(initial_len);
                Expr::FunType(args, Arc::new(ret))
            }
            Expr::FunExpr(args, body) => {
                let initial_len = self.local_env.len();
                let args = args
                    .iter()
                    .map(|FunArg { pat, ty }| {
                        let pat = pat.clone();
                        let ty = self.zonk_expr(ty);
                        self.subst_pat(&pat);
                        FunArg { pat, ty }
                    })
                    .collect();
                let body = self.zonk_expr(body);
                self.local_env.truncate(initial_len);
                Expr::FunExpr(args, Arc::new(body))
            }
            Expr::Let(pat, ty, init, body) => {
                let initial_len = self.local_env.len();
                let ty = self.zonk_expr(ty);
                let init = self.zonk_expr(init);
                self.subst_pat(pat);
                let body = self.zonk_expr(body);
                self.local_env.truncate(initial_len);
                Expr::Let(pat.clone(), Arc::new(ty), Arc::new(init), Arc::new(body))
            }
        }
    }

    #[debug_requires(expr.is_closed(self.local_env.len(), self.meta_env.len()))]
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    fn zonk_spine(&mut self, expr: &Expr) -> Either<Expr, Arc<Value>> {
        match expr {
            Expr::Meta(var) => match self.meta_env.get(*var) {
                Some(Some(value)) => Right(value.clone()),
                Some(None) => Left(Expr::Error),
                None => unreachable!("Unbound meta variable"),
            },
            Expr::MetaInsertion(var, sources) => match self.meta_env.get(*var) {
                Some(Some(value)) => {
                    Right(self.fold_local_sources(value.clone(), sources.iter().copied()))
                }
                Some(None) => Left(Expr::Error),
                None => unreachable!("Unbound meta variable"),
            },
            Expr::FunCall(fun, args) => match self.zonk_spine(fun) {
                Left(fun) => {
                    let args = args.iter().map(|arg| self.zonk_expr(arg)).collect();
                    Left(Expr::FunCall(Arc::new(fun), args))
                }
                Right(fun) => {
                    let args = args.iter().map(|arg| self.eval_expr(arg)).collect();
                    Right(self.elim_ctx().do_fun_call(fun, args))
                }
            },
            Expr::Match(scrut, arms) => match self.zonk_spine(scrut) {
                Left(scrut) => {
                    let arms = arms
                        .iter()
                        .map(|(pat, expr)| {
                            let expr = self.zonk_expr(expr);
                            (pat.clone(), expr)
                        })
                        .collect();
                    Left(Expr::Match(Arc::new(scrut), arms))
                }
                Right(scrut) => {
                    let arms = MatchArms::new(self.local_env.clone(), arms.clone());
                    Right(self.elim_ctx().do_match(scrut, arms))
                }
            },
            expr => Left(self.zonk_expr(expr)),
        }
    }

    pub fn subst_pat(&mut self, pat: &Pat) {
        let var = || Arc::new(Value::local(self.local_env.len().to_level()));
        match pat {
            Pat::Error | Pat::Lit(_) | Pat::Name(_) => self.local_env.push(var()),
            Pat::Variant(_, pats) => pats.iter().for_each(|pat| self.subst_pat(pat)),
        }
    }
}

pub struct ElimCtx<'env> {
    meta_env: &'env UniqueEnv<Option<Arc<Value>>>,
    db: &'env dyn crate::Db,
}

impl<'env> ElimCtx<'env> {
    pub fn new(meta_env: &'env UniqueEnv<Option<Arc<Value>>>, db: &'env dyn crate::Db) -> Self {
        Self { meta_env, db }
    }

    pub fn eval_ctx(&self, local_values: &'env mut SharedEnv<Arc<Value>>) -> EvalCtx<'env> {
        EvalCtx::new(local_values, self.meta_env, self.db)
    }

    pub fn force_value(&self, value: &Arc<Value>) -> Arc<Value> {
        let mut forced_value = value.clone();
        while let Value::Stuck(Head::Meta(level), spine) = forced_value.as_ref() {
            match self.meta_env.get(*level) {
                Some(Some(value)) => forced_value = self.apply_spine(value.clone(), spine),
                Some(None) => break,
                None => unreachable!("Unbound meta variable: {level:?}"),
            }
        }
        forced_value
    }

    fn apply_spine(&self, head: Arc<Value>, spine: &[Elim]) -> Arc<Value> {
        spine.iter().fold(head, |head, elim| match elim {
            Elim::FunCall(args) => self.do_fun_call(head, args.clone()),
            Elim::Match(arms) => self.do_match(head, arms.clone()),
        })
    }

    pub fn do_fun_call(&self, mut fun: Arc<Value>, args: Vec<Arc<Value>>) -> Arc<Value> {
        if fun.as_ref() == &Value::Error {
            return fun;
        }

        match Arc::make_mut(&mut fun) {
            Value::FunValue(closure) => self.apply_closure(closure, args),
            Value::Stuck(_, spine) => {
                spine.push(Elim::FunCall(args));
                fun
            }
            _ => unreachable!("tried to call non-fun value: {fun:?}"),
        }
    }

    pub fn do_match(&self, mut scrut: Arc<Value>, arms: MatchArms) -> Arc<Value> {
        match Arc::make_mut(&mut scrut) {
            Value::Error => return scrut,
            Value::Stuck(_, spine) => {
                spine.push(Elim::Match(arms));
                return scrut;
            }
            _ => {}
        };

        let MatchArms { mut env, arms } = arms;
        for (pat, expr) in arms.iter() {
            match (pat, scrut.clone()) {
                (Pat::Error, _) => {
                    env.push(scrut);
                    return Arc::new(Value::Error);
                }
                (Pat::Name(_), scrut) => {
                    env.push(scrut);
                    return self.eval_ctx(&mut env).eval_expr(expr);
                }
                (Pat::Lit(lit1), scrut) if scrut.as_ref() == &Value::Lit(lit1.clone()) => {
                    env.push(scrut);
                    return self.eval_ctx(&mut env).eval_expr(expr);
                }

                (Pat::Lit(_), _) => continue,
                (Pat::Variant(..), _) => todo!(),
            }
        }

        unreachable!("non-exhaustive match: {scrut:?}")
    }

    pub fn apply_closure(&self, closure: &FunClosure, args: Vec<Arc<Value>>) -> Arc<Value> {
        assert_eq!(closure.arity(), args.len());
        let mut env = closure.env.clone();
        env.extend(args);
        self.eval_ctx(&mut env).eval_expr(&closure.body)
    }

    pub fn split_fun_closure(
        &self,
        mut closure: FunClosure,
    ) -> Option<(FunArg<Arc<Value>>, impl FnOnce(Arc<Value>) -> FunClosure)> {
        let (FunArg { pat, ty }, args) = closure.args.split_first()?;
        let mut ctx = self.eval_ctx(&mut closure.env);
        let ty = ctx.eval_expr(ty);

        let args = Arc::from(args);
        Some((
            FunArg {
                pat: pat.clone(),
                ty,
            },
            move |prev| {
                closure.env.push(prev);
                closure.args = args;
                closure
            },
        ))
    }

    pub fn split_arms(&self, mut arms: MatchArms) -> Option<(Pat, Arc<Value>, MatchArms)> {
        match arms.arms.split_first() {
            None => None,
            Some((first, rest)) => {
                let first = first.clone();
                arms.arms = Arc::from(rest);

                let mut ctx = self.eval_ctx(&mut arms.env);
                let (pat, expr) = first;
                Some((pat, ctx.eval_expr(&expr), arms))
            }
        }
    }
}
