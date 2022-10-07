use std::sync::Arc;

use contracts::debug_ensures;

use super::env::{LocalSource, SharedEnv, UniqueEnv};
use super::quote::QuoteCtx;
use super::syntax::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct EvalOpts {
    /// TODO: the case where `beta_reduce == false` (embed Expr in Value?)
    /// * Replace `Expr::FunCall` with the result of applying the arguments to
    ///   the function
    /// * Replace `Expr::Match` with the result of pattern matching
    pub beta_reduce: bool,

    /// * Replace `Expr::Local` with its value from the local environment
    /// * Replace `Expr::Meta` with its value from the meta environment
    /// * Replace `Expr::LetDef` with its value from the global environment
    pub delta_reduce: bool,

    /// Replace unsoled `Expr::Meta`s with `Value::Error` instead of
    /// `Value::Meta`
    pub error_on_unsolved_meta: bool,

    /// TODO: the case where `zeta_reduce == false` (embed Expr in Value?)
    /// Replace `Expr::Let` with the result of evaluating the `body` expression
    /// in an environment extended with the `init` expression
    pub zeta_reduce: bool,

    /// Reduce the body of an `Expr::FunType`/`Expr::FunExpr` when creating a
    /// closure
    pub reduce_under_lambda: bool,

    /// Reduce type annotations that do not exist "at runtime" but should still
    /// be fully reduced when doing tasks such as zonking
    pub reduce_annotations: bool,

    /// TODO: the case where `strict == false`
    /// Reduce an expression as far as possible when:
    /// * Binding the pattern in an `Expr::Let`
    /// * Calling the function in an `Expr::FunCall`
    /// * Applying the arguments in an `Expr::FunCall`
    /// * Reducing the scrutinee of an `Expr::Match`
    /// Otherwise, capture the enclosing environment in a `LazyClosure` and
    /// reduce only when demanded by an `Expr::Match` branch.
    pub strict: bool,
}

impl EvalOpts {
    pub const EVAL_CBV: Self = Self {
        delta_reduce: true,
        zeta_reduce: true,
        error_on_unsolved_meta: false,
        beta_reduce: true,
        reduce_under_lambda: false,
        reduce_annotations: false,
        strict: true,
    };
    pub const EVAL_CBN: Self = Self {
        delta_reduce: true,
        zeta_reduce: true,
        error_on_unsolved_meta: false,
        beta_reduce: true,
        reduce_under_lambda: false,
        reduce_annotations: false,
        strict: false,
    };
    pub const ZONK: Self = Self {
        delta_reduce: false,
        zeta_reduce: false,
        error_on_unsolved_meta: true,
        beta_reduce: false,
        reduce_under_lambda: true,
        reduce_annotations: true,
        strict: true,
    };
    pub const PARTIAL_EVAL: Self = Self {
        delta_reduce: true,
        zeta_reduce: true,
        error_on_unsolved_meta: true,
        beta_reduce: true,
        reduce_under_lambda: true,
        reduce_annotations: false,
        strict: true,
    };
}

pub struct EvalCtx<'env> {
    local_env: &'env mut SharedEnv<Arc<Value>>,
    meta_env: &'env UniqueEnv<Option<Arc<Value>>>,
    opts: EvalOpts,
    db: &'env dyn crate::Db,
}

impl<'env> EvalCtx<'env> {
    pub fn new(
        local_env: &'env mut SharedEnv<Arc<Value>>,
        meta_env: &'env UniqueEnv<Option<Arc<Value>>>,
        opts: EvalOpts,
        db: &'env dyn crate::Db,
    ) -> Self {
        Self {
            local_env,
            meta_env,
            opts,
            db,
        }
    }

    pub fn with_opts(mut self, opts: EvalOpts) -> Self {
        self.opts = opts;
        self
    }

    fn elim_ctx(&self) -> ElimCtx { ElimCtx::new(self.meta_env, self.opts, self.db) }

    fn quote_ctx(&self) -> QuoteCtx { QuoteCtx::new(self.local_env.len(), self.meta_env, self.db) }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    pub fn normalize_expr(&mut self, expr: &Expr) -> (Expr, Arc<Value>) {
        let value = self.eval_expr(expr);
        let expr = self.quote_ctx().quote_value(&value);
        (expr, value)
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    pub fn eval_expr(&mut self, expr: &Expr) -> Arc<Value> {
        match expr {
            Expr::Error => Arc::new(Value::Error),
            Expr::Type => Arc::new(Value::Type),
            Expr::BoolType => Arc::new(Value::BoolType),
            Expr::Lit(lit) => Arc::new(Value::Lit(lit.clone())),
            Expr::LetDef(def) if self.opts.delta_reduce => {
                let def = crate::core::elab::elab_let_def(self.db, *def);
                def.body.1
            }
            Expr::LetDef(def) => Arc::new(Value::let_def(*def)),
            Expr::EnumDef(enum_def) => Arc::new(Value::enum_def(*enum_def)),
            Expr::EnumVariant(enum_variant) => Arc::new(Value::enum_variant(*enum_variant)),
            Expr::Local(index) if self.opts.delta_reduce => match self.local_env.get(*index) {
                Some(value) => value.clone(),
                None => unreachable!("Unbound local variable: {index:?}"),
            },
            Expr::Local(index) => Arc::new(Value::local(
                self.local_env
                    .len()
                    .index_to_level(*index)
                    .unwrap_or_else(|| unreachable!("Unbound local variable: {index:?}")),
            )),
            Expr::Meta(level) if self.opts.delta_reduce => match self.meta_env.get(*level) {
                Some(Some(value)) => value.clone(),
                Some(None) if self.opts.error_on_unsolved_meta => Arc::new(Value::Error),
                Some(None) => Arc::new(Value::meta(*level)),
                None => unreachable!("Unbound meta variable: {level:?}"),
            },
            Expr::Meta(level) => Arc::new(Value::meta(*level)),

            Expr::MetaInsertion(level, sources) => {
                let mut head = self.eval_expr(&Expr::Meta(*level));
                for (source, value) in sources.iter().zip(self.local_env.iter()) {
                    head = match source {
                        LocalSource::Def => head,
                        LocalSource::Param => {
                            self.elim_ctx().do_fun_call(head, vec![value.clone()])
                        }
                    };
                }
                head
            }
            Expr::FunType(args, ret) if self.opts.reduce_under_lambda => {
                let initial_len = self.local_env.len();
                let args = args
                    .iter()
                    .map(|FunArg { pat, ty }| {
                        let ty = self.eval_expr(ty);
                        let ty = self.quote_ctx().quote_value(&ty);
                        let arg = Arc::new(Value::local(self.local_env.len().to_level()));
                        self.local_env.push(arg);
                        FunArg {
                            pat: pat.clone(),
                            ty,
                        }
                    })
                    .collect();
                let ret = self.eval_expr(ret);
                let ret = self.quote_ctx().quote_value(&ret);
                self.local_env.truncate(initial_len);
                Arc::new(Value::FunType(FunClosure::new(
                    self.local_env.clone(),
                    args,
                    Arc::new(ret),
                )))
            }
            Expr::FunType(args, ret) => Arc::new(Value::FunType(FunClosure::new(
                self.local_env.clone(),
                args.clone(),
                ret.clone(),
            ))),
            Expr::FunExpr(args, ret) if self.opts.reduce_under_lambda => {
                let initial_len = self.local_env.len();
                let args = args
                    .iter()
                    .map(|FunArg { pat, ty }| {
                        let ty = if self.opts.reduce_annotations {
                            let ty = self.eval_expr(ty);
                            let ty = self.quote_ctx().quote_value(&ty);
                            ty
                        } else {
                            ty.clone()
                        };
                        let arg = Arc::new(Value::local(self.local_env.len().to_level()));
                        self.local_env.push(arg);
                        FunArg {
                            pat: pat.clone(),
                            ty,
                        }
                    })
                    .collect();
                let ret = self.eval_expr(ret);
                let ret = self.quote_ctx().quote_value(&ret);
                self.local_env.truncate(initial_len);
                Arc::new(Value::FunType(FunClosure::new(
                    self.local_env.clone(),
                    args,
                    Arc::new(ret),
                )))
            }
            Expr::FunExpr(args, body) => Arc::new(Value::FunValue(FunClosure::new(
                self.local_env.clone(),
                args.clone(),
                body.clone(),
            ))),
            Expr::FunCall(fun, args) if self.opts.beta_reduce => {
                let fun = self.eval_expr(fun);
                let args = args.iter().map(|arg| self.eval_expr(arg)).collect();
                self.elim_ctx().do_fun_call(fun, args)
            }
            Expr::FunCall(..) => todo!(),
            Expr::Let(_, _, init, body) if self.opts.zeta_reduce => {
                let init_value = self.eval_expr(init);
                self.local_env.push(init_value);
                let body_value = self.eval_expr(body);
                self.local_env.pop();
                body_value
            }
            Expr::Let(..) => todo!(),
            Expr::Match(scrut, arms) if self.opts.beta_reduce => {
                let scrut = self.eval_expr(scrut);
                let arms = MatchArms::new(self.local_env.clone(), arms.clone());
                self.elim_ctx().do_match(scrut, arms)
            }
            Expr::Match(..) => todo!(),
        }
    }
}

pub struct ElimCtx<'env> {
    meta_env: &'env UniqueEnv<Option<Arc<Value>>>,
    opts: EvalOpts,
    db: &'env dyn crate::Db,
}

impl<'env> ElimCtx<'env> {
    pub fn new(
        meta_env: &'env UniqueEnv<Option<Arc<Value>>>,
        opts: EvalOpts,
        db: &'env dyn crate::Db,
    ) -> Self {
        Self { meta_env, opts, db }
    }

    pub fn eval_ctx(&self, local_values: &'env mut SharedEnv<Arc<Value>>) -> EvalCtx<'env> {
        EvalCtx::new(local_values, self.meta_env, self.opts, self.db)
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
