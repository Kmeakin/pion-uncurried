use std::sync::Arc;

use contracts::{debug_ensures, debug_requires};
use either::Either;
use either::Either::*;

use super::elab::eval_let_def_expr;
use super::env::{EnvLen, LocalSource, SharedEnv, UniqueEnv};
use super::syntax::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct EvalFlags {
    unfold_locals: bool,
    unfold_globals: bool,
    unfold_metas: bool,
    error_on_unsolved_metas: bool,
}

impl EvalFlags {
    pub const EVAL: Self = Self {
        unfold_locals: true,
        unfold_globals: true,
        unfold_metas: true,
        error_on_unsolved_metas: false,
    };
    pub const ZONK: Self = Self {
        unfold_locals: false,
        unfold_globals: false,
        unfold_metas: true,
        error_on_unsolved_metas: true,
    };
}

pub type LocalEnv = SharedEnv<Arc<Value>>;
pub type MetaEnv = UniqueEnv<Option<Arc<Value>>>;

pub struct EvalCtx<'env> {
    local_env: &'env mut LocalEnv,
    meta_env: &'env MetaEnv,
    db: &'env dyn crate::Db,
    flags: EvalFlags,
}

impl<'env> EvalCtx<'env> {
    pub fn new(
        local_env: &'env mut LocalEnv,
        meta_env: &'env MetaEnv,
        db: &'env dyn crate::Db,
        flags: EvalFlags,
    ) -> Self {
        Self {
            local_env,
            meta_env,
            db,
            flags,
        }
    }

    pub fn with_flags(self, flags: EvalFlags) -> Self { Self { flags, ..self } }

    fn elim_ctx(&self) -> ElimCtx { ElimCtx::new(self.meta_env, self.db, self.flags) }

    fn quote_ctx(&self) -> QuoteCtx {
        QuoteCtx::new(self.local_env.len(), self.meta_env, self.db, self.flags)
    }

    #[debug_requires(expr.is_closed(self.local_env.len(), self.meta_env.len()))]
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    pub fn eval_expr(&mut self, expr: &Expr) -> Arc<Value> {
        match expr {
            Expr::Prim(prim) => Arc::new(Value::prim(prim.clone())),
            Expr::Lit(lit) => Arc::new(Value::Lit(lit.clone())),
            Expr::Local(var) if self.flags.unfold_locals => match self.local_env.get(*var) {
                Some(value) => value.clone(),
                None => unreachable!("Unbound local variable: {var:?}"),
            },
            Expr::Local(var) => match self.local_env.len().index_to_level(*var) {
                Some(var) => Arc::new(Value::local(var)),
                None => unreachable!("Unbound local variable: {var:?}"),
            },
            Expr::Global(GlobalVar::Let(var)) if self.flags.unfold_globals => {
                eval_let_def_expr(self.db, *var)
            }
            Expr::Global(var) => Arc::new(Value::global(*var)),
            Expr::Meta(var) if self.flags.unfold_metas => match self.meta_env.get(*var) {
                Some(Some(value)) => value.clone(),
                Some(None) if self.flags.error_on_unsolved_metas => Arc::new(Value::ERROR),
                Some(None) => Arc::new(Value::meta(*var)),
                None => unreachable!("Unbound meta variable: {var:?}"),
            },
            Expr::Meta(var) => Arc::new(Value::meta(*var)),
            Expr::MetaInsertion(var, sources) => {
                let head = self.eval_expr(&Expr::Meta(*var));
                self.apply_local_sources(head, sources)
            }
            Expr::FunType(args, ret) => {
                let closure = FunClosure::new(self.local_env.clone(), args.clone(), ret.clone());
                Arc::new(Value::FunType(closure))
            }
            Expr::FunExpr(args, body) => {
                let closure = FunClosure::new(self.local_env.clone(), args.clone(), body.clone());
                Arc::new(Value::FunValue(closure))
            }
            Expr::FunCall(fun, args) => {
                let fun = self.eval_expr(fun);
                let args = args.iter().map(|arg| self.eval_expr(arg)).collect();
                self.elim_ctx().call_fun_value(fun, args)
            }
            Expr::Match(scrut, arms) => {
                let scrut = self.eval_expr(scrut);
                let arms = MatchArms::new(self.local_env.clone(), arms.clone());
                self.elim_ctx().apply_match_arms(scrut, arms)
            }
            Expr::Let(pat, _, init, body) => {
                let initial_len = self.local_env.len();
                let init = self.eval_expr(init);
                self.local_env.subst_value_into_pat(pat, init);
                let ret = self.eval_expr(body);
                self.local_env.truncate(initial_len);
                ret
            }
        }
    }

    fn apply_local_sources(
        &mut self,
        mut head: Arc<Value>,
        sources: &SharedEnv<LocalSource>,
    ) -> Arc<Value> {
        for (source, value) in sources.iter().zip(self.local_env.iter()) {
            head = match source {
                LocalSource::Def => head,
                LocalSource::Param => self.elim_ctx().call_fun_value(head, vec![value.clone()]),
            }
        }
        head
    }

    #[debug_requires(expr.is_closed(self.local_env.len(), self.meta_env.len()))]
    #[debug_ensures(ret.is_closed(self.local_env.len(), EnvLen(0)))]
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    pub fn zonk_expr(&mut self, expr: &Expr) -> Expr {
        match expr {
            Expr::Prim(_) | Expr::Lit(_) | Expr::Local(_) | Expr::Global(_) => expr.clone(),
            Expr::Meta(..) | Expr::MetaInsertion(..) | Expr::FunCall(..) | Expr::Match(..) => {
                match self.zonk_spine(expr) {
                    Left(expr) => expr,
                    Right(value) => self.quote_ctx().quote_value(&value),
                }
            }
            Expr::FunType(args, body) => {
                let initial_len = self.local_env.len();
                let args = args
                    .iter()
                    .map(|arg| {
                        let ty = self.zonk_expr(&arg.ty);
                        self.local_env.subst_arg_into_pat(&arg.pat);
                        FunArg {
                            pat: arg.pat.clone(),
                            ty,
                        }
                    })
                    .collect();
                let body = self.zonk_expr(body);
                self.local_env.truncate(initial_len);
                Expr::FunType(args, Arc::new(body))
            }
            Expr::FunExpr(args, body) => {
                let initial_len = self.local_env.len();
                let args = args
                    .iter()
                    .map(|arg| {
                        let ty = self.zonk_expr(&arg.ty);
                        self.local_env.subst_arg_into_pat(&arg.pat);
                        FunArg {
                            pat: arg.pat.clone(),
                            ty,
                        }
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
                self.local_env.subst_arg_into_pat(pat);
                let body = self.zonk_expr(body);
                self.local_env.truncate(initial_len);
                Expr::Let(pat.clone(), Arc::new(ty), Arc::new(init), Arc::new(body))
            }
        }
    }

    fn zonk_spine(&mut self, expr: &Expr) -> Either<Expr, Arc<Value>> {
        match expr {
            Expr::Meta(var) => match self.meta_env.get(*var) {
                Some(Some(value)) => Right(value.clone()),
                Some(None) => Left(Expr::ERROR),
                None => unreachable!("Unbound meta variable: {var:?}"),
            },
            Expr::MetaInsertion(var, sources) => match self.meta_env.get(*var) {
                Some(Some(value)) => {
                    let value = self.apply_local_sources(value.clone(), sources);
                    Right(value)
                }
                Some(None) => Left(Expr::ERROR),
                None => unreachable!("Unbound meta variable: {var:?}"),
            },
            Expr::FunCall(fun, args) => match self.zonk_spine(fun) {
                Left(expr) => Left(Expr::FunCall(
                    Arc::new(expr),
                    args.iter().map(|arg| self.zonk_expr(arg)).collect(),
                )),
                Right(fun) => {
                    let args = args.iter().map(|arg| self.eval_expr(arg)).collect();
                    Right(self.elim_ctx().call_fun_value(fun, args))
                }
            },
            Expr::Match(scrut, arms) => match self.zonk_spine(scrut) {
                Left(scrut) => {
                    let initial_len = self.local_env.len();
                    let arms = arms
                        .iter()
                        .map(|(pat, expr)| {
                            self.local_env.subst_arg_into_pat(pat);
                            (pat.clone(), self.zonk_expr(expr))
                        })
                        .collect();
                    self.local_env.truncate(initial_len);
                    Left(Expr::Match(Arc::new(scrut), arms))
                }
                Right(value) => {
                    let arms = MatchArms::new(self.local_env.clone(), arms.clone());
                    Right(self.elim_ctx().apply_match_arms(value, arms))
                }
            },
            _ => Left(self.zonk_expr(expr)),
        }
    }
}

pub struct ElimCtx<'env> {
    meta_env: &'env MetaEnv,
    db: &'env dyn crate::Db,
    flags: EvalFlags,
}

impl<'env> ElimCtx<'env> {
    pub fn new(meta_env: &'env MetaEnv, db: &'env dyn crate::Db, flags: EvalFlags) -> Self {
        Self {
            meta_env,
            db,
            flags,
        }
    }

    fn eval_ctx(&self, local_values: &'env mut SharedEnv<Arc<Value>>) -> EvalCtx<'env> {
        EvalCtx::new(local_values, self.meta_env, self.db, self.flags)
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
            Elim::FunCall(args) => self.call_fun_value(head, args.clone()),
            Elim::Match(arms) => self.apply_match_arms(head, arms.clone()),
        })
    }

    pub fn call_fun_value(&self, mut fun: Arc<Value>, args: Vec<Arc<Value>>) -> Arc<Value> {
        match Arc::make_mut(&mut fun) {
            Value::Stuck(_, spine) => {
                spine.push(Elim::FunCall(args));
                fun
            }
            Value::FunValue(closure) => self.call_fun_closure(closure, args),
            _ => unreachable!("Cannot call non function value `{fun:?}`"),
        }
    }

    pub fn call_fun_closure(&self, closure: &FunClosure, args: Vec<Arc<Value>>) -> Arc<Value> {
        assert_eq!(closure.arity(), args.len());
        let mut env = closure.env.clone();
        for (arg, value) in closure.args.iter().zip(args.into_iter()) {
            env.subst_value_into_pat(&arg.pat, value);
        }

        self.eval_ctx(&mut env).eval_expr(&closure.body)
    }

    pub fn apply_match_arms(&self, mut scrut: Arc<Value>, arms: MatchArms) -> Arc<Value> {
        if let Value::Stuck(_, spine) = Arc::make_mut(&mut scrut) {
            spine.push(Elim::Match(arms));
            return scrut;
        };

        let MatchArms { mut env, arms } = arms;
        for (pat, expr) in arms.iter() {
            match pat {
                Pat::Error => {
                    env.subst_value_into_pat(pat, scrut);
                    return self.eval_ctx(&mut env).eval_expr(expr);
                }
                Pat::Name(_) => {
                    env.subst_value_into_pat(pat, scrut);
                    return self.eval_ctx(&mut env).eval_expr(expr);
                }
                Pat::Lit(lit1) if scrut.as_ref() == &Value::Lit(lit1.clone()) => {
                    env.subst_value_into_pat(pat, scrut);
                    return self.eval_ctx(&mut env).eval_expr(expr);
                }
                Pat::Lit(_) => continue,
                Pat::Variant(..) => todo!(),
            }
        }

        unreachable!("non-exhaustive match: {scrut:?}")
    }

    pub fn split_fun_closure(
        &self,
        mut closure: FunClosure,
    ) -> Option<(FunArg<Arc<Value>>, impl FnOnce(Arc<Value>) -> FunClosure)> {
        let (FunArg { pat, ty }, args) = closure.args.split_first()?;
        let pat = pat.clone();
        let args = Arc::from(args);
        let ty = self.eval_ctx(&mut closure.env).eval_expr(ty);

        let arg = FunArg {
            pat: pat.clone(),
            ty,
        };
        let cont = move |prev| {
            closure.env.subst_value_into_pat(&pat, prev);
            closure.args = args;
            closure
        };
        Some((arg, cont))
    }

    pub fn split_match_arms(
        &self,
        mut arms: MatchArms,
    ) -> Option<((Pat, Arc<Value>), impl FnOnce(Arc<Value>) -> MatchArms)> {
        let ((pat, expr), rest) = arms.arms.split_first()?;
        let pat = pat.clone();
        let rest = Arc::from(rest);
        let value = self.eval_ctx(&mut arms.env).eval_expr(expr);
        let first = (pat.clone(), value);

        let cont = move |prev| {
            arms.env.subst_value_into_pat(&pat, prev);
            arms.arms = rest;
            arms
        };
        Some((first, cont))
    }
}

pub struct QuoteCtx<'env> {
    local_len: EnvLen,
    meta_env: &'env MetaEnv,
    db: &'env dyn crate::Db,
    flags: EvalFlags,
}

impl<'env> QuoteCtx<'env> {
    pub fn new(
        local_len: EnvLen,
        meta_env: &'env MetaEnv,
        db: &'env dyn crate::Db,
        flags: EvalFlags,
    ) -> Self {
        Self {
            local_len,
            meta_env,
            db,
            flags,
        }
    }

    fn elim_ctx(&self) -> ElimCtx { ElimCtx::new(self.meta_env, self.db, self.flags) }

    #[debug_ensures(self.local_len == old(self.local_len))]
    #[debug_ensures(ret.is_closed(self.local_len, self.meta_env.len()))]
    pub fn quote_value(&mut self, value: &Value) -> Expr {
        match value {
            Value::Lit(lit) => Expr::Lit(lit.clone()),
            Value::Stuck(head, spine) => {
                let head_expr = match head {
                    Head::Local(var) => match self.local_len.level_to_index(*var) {
                        Some(var) => Expr::Local(var),
                        None => unreachable!("Unbound local variable: {var:?}"),
                    },
                    Head::Meta(var) => Expr::Meta(*var),
                    Head::Prim(prim) => Expr::Prim(prim.clone()),
                    Head::Global(var) => Expr::Global(*var),
                };
                spine.iter().fold(head_expr, |head_core, elim| match elim {
                    Elim::FunCall(args) => {
                        let args = args.iter().map(|arg| self.quote_value(arg)).collect();
                        Expr::FunCall(Arc::new(head_core), args)
                    }
                    Elim::Match(arms) => {
                        let mut arms = arms.clone();
                        let mut core_arms = Vec::with_capacity(arms.arms.len());
                        while let Some(((pat, value), cont)) =
                            self.elim_ctx().split_match_arms(arms.clone())
                        {
                            core_arms.push((pat, self.quote_value(&value)));
                            arms = cont(value);
                        }
                        Expr::Match(Arc::new(head_core), Arc::from(core_arms))
                    }
                })
            }
            Value::FunType(closure) => {
                let (args, body) = self.quote_closure(closure);
                Expr::FunType(args, Arc::new(body))
            }
            Value::FunValue(closure) => {
                let (args, body) = self.quote_closure(closure);
                Expr::FunExpr(args, Arc::new(body))
            }
        }
    }

    #[debug_ensures(self.local_len == old(self.local_len))]
    fn quote_closure(&mut self, closure: &FunClosure) -> (Arc<[FunArg<Expr>]>, Expr) {
        let start_len = self.local_len;

        let initial_closure = closure.clone();
        let mut closure = closure.clone();
        let mut fun_args = Vec::with_capacity(closure.arity());
        let mut arg_values = Vec::with_capacity(closure.arity());

        while let Some((FunArg { pat, ty }, cont)) =
            self.elim_ctx().split_fun_closure(closure.clone())
        {
            let arg_value = Arc::new(Value::local(self.local_len.to_level()));
            closure = cont(arg_value.clone());
            let type_core = self.quote_value(&ty);
            self.local_len.subst_pat(&pat);
            fun_args.push(FunArg { pat, ty: type_core });
            arg_values.push(arg_value);
        }

        let body = self
            .elim_ctx()
            .call_fun_closure(&initial_closure, arg_values);
        let body = self.quote_value(&body);
        self.local_len.truncate(start_len);

        (Arc::from(fun_args), body)
    }
}

impl LocalEnv {
    /// Push an `Value::local` onto env for each binder in `pat`.
    pub fn subst_arg_into_pat(&mut self, pat: &Pat) {
        let var = || Arc::new(Value::local(self.len().to_level()));
        match pat {
            Pat::Error | Pat::Lit(_) | Pat::Name(_) => self.push(var()),
            Pat::Variant(_, pats) => pats.iter().for_each(|pat| self.subst_arg_into_pat(pat)),
        }
    }

    pub fn subst_value_into_pat(&mut self, pat: &Pat, value: Arc<Value>) {
        match pat {
            Pat::Error | Pat::Lit(_) | Pat::Name(_) => self.push(value),
            Pat::Variant(variant, pats) => match value.as_ref() {
                Value::Stuck(Head::Global(GlobalVar::Enum(v)), spine) => {
                    if let [Elim::FunCall(args)] = spine.as_slice() {
                        let args = args.iter().cloned();
                        for (pat, arg) in pats.iter().zip(args) {
                            self.subst_value_into_pat(pat, arg);
                        }
                    } else {
                        unreachable!()
                    }
                }
                _ => todo!("Cannot subst {value:?} into {pat:?}"),
            },
        }
    }
}

impl EnvLen {
    pub fn subst_pat(&mut self, pat: &Pat) { *self += pat.num_binders(); }
}
