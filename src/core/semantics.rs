use std::sync::Arc;

use contracts::{debug_ensures, debug_requires};
use either::Either;
use either::Either::*;

use self::binders::IsClosed;
use super::elab::eval_let_def_expr;
use super::env::{EnvLen, LocalSource, SharedEnv, UniqueEnv};
use super::syntax::*;

pub mod binders;
pub mod subst;

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

    fn elim_ctx(&self) -> ElimCtx {
        ElimCtx::new(self.local_env.len(), self.meta_env, self.db, self.flags)
    }

    fn quote_ctx(&self) -> QuoteCtx {
        QuoteCtx::new(self.local_env.len(), self.meta_env, self.db, self.flags)
    }

    #[track_caller]
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
                self.elim_ctx().apply_fun_value(fun, args)
            }
            Expr::Match(scrut, branches) => {
                let scrut = self.eval_expr(scrut);
                let closure = MatchClosure::new(self.local_env.clone(), branches.clone());
                self.elim_ctx().apply_match_closure(scrut, closure)
            }
            Expr::Let(pat, _, init, body) => {
                let initial_len = self.local_env.len();
                let init = self.eval_expr(init);
                self.push_pat_defs(pat, init);
                let ret = self.eval_expr(body);
                self.local_env.truncate(initial_len);
                ret
            }
        }
    }

    #[track_caller]
    #[debug_requires(head.is_closed(self.local_env.len(), self.meta_env.len()))]
    #[debug_ensures(ret.is_closed(self.local_env.len(), self.meta_env.len()))]
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    fn apply_local_sources(
        &mut self,
        head: Arc<Value>,
        sources: &SharedEnv<LocalSource>,
    ) -> Arc<Value> {
        let mut head = head;
        for (source, value) in sources.iter().zip(self.local_env.iter()) {
            head = match source {
                LocalSource::Def => head,
                LocalSource::Param => self.elim_ctx().apply_fun_value(head, vec![value.clone()]),
            }
        }
        head
    }

    #[track_caller]
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
                let args = self.zonk_telescope(args);
                let body = self.zonk_expr(body);
                self.local_env.truncate(initial_len);
                Expr::FunType(args, Arc::new(body))
            }
            Expr::FunExpr(args, body) => {
                let initial_len = self.local_env.len();
                let args = self.zonk_telescope(args);
                let body = self.zonk_expr(body);
                self.local_env.truncate(initial_len);
                Expr::FunExpr(args, Arc::new(body))
            }
            Expr::Let(pat, r#type, init, body) => {
                let initial_len = self.local_env.len();
                let r#type = self.zonk_expr(r#type);
                let init = self.zonk_expr(init);
                self.push_pat_params(pat);
                let body = self.zonk_expr(body);
                self.local_env.truncate(initial_len);
                Expr::Let(
                    pat.clone(),
                    Arc::new(r#type),
                    Arc::new(init),
                    Arc::new(body),
                )
            }
        }
    }

    #[track_caller]
    #[debug_requires(expr.is_closed(self.local_env.len(), self.meta_env.len()))]
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
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
                    Right(self.elim_ctx().apply_fun_value(fun, args))
                }
            },
            Expr::Match(scrut, branches) => match self.zonk_spine(scrut) {
                Left(scrut) => {
                    let initial_len = self.local_env.len();
                    let branches = branches
                        .iter()
                        .map(|(pat, expr)| {
                            self.push_pat_params(pat);
                            (pat.clone(), self.zonk_expr(expr))
                        })
                        .collect();
                    self.local_env.truncate(initial_len);
                    Left(Expr::Match(Arc::new(scrut), branches))
                }
                Right(value) => {
                    let closure = MatchClosure::new(self.local_env.clone(), branches.clone());
                    Right(self.elim_ctx().apply_match_closure(value, closure))
                }
            },
            _ => Left(self.zonk_expr(expr)),
        }
    }

    fn zonk_fun_arg(&mut self, arg: &FunArg<Expr>) -> FunArg<Expr> {
        let FunArg { pat, r#type } = arg;
        let r#type = self.zonk_expr(r#type);
        self.push_pat_params(pat);
        FunArg {
            pat: pat.clone(),
            r#type,
        }
    }

    fn zonk_telescope(&mut self, telescope: &Telescope<Expr>) -> Telescope<Expr> {
        telescope.iter().map(|arg| self.zonk_fun_arg(arg)).collect()
    }
}

pub struct ElimCtx<'env> {
    /// Only used for `debug_ensures` / `debug_requires`
    local_env: EnvLen,
    meta_env: &'env MetaEnv,
    db: &'env dyn crate::Db,
    flags: EvalFlags,
}

impl<'env> ElimCtx<'env> {
    pub fn new(
        local_env: EnvLen,
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

    #[track_caller]
    #[debug_requires(head.is_closed(self.local_env, self.meta_env.len()))]
    #[debug_ensures(ret.is_closed(self.local_env, self.meta_env.len()))]
    fn apply_spine(&self, head: Arc<Value>, spine: &[Elim]) -> Arc<Value> {
        spine.iter().fold(head, |head, elim| match elim {
            Elim::FunCall(args) => self.apply_fun_value(head, args.clone()),
            Elim::Match(closure) => self.apply_match_closure(head, closure.clone()),
        })
    }

    #[track_caller]
    pub fn apply_fun_value(&self, fun: Arc<Value>, args: Vec<Arc<Value>>) -> Arc<Value> {
        let mut fun = fun;
        match Arc::make_mut(&mut fun) {
            Value::Stuck(_, spine) => {
                spine.push(Elim::FunCall(args));
                fun
            }
            Value::FunValue(closure) => self.apply_fun_closure(closure, args),
            _ => unreachable!("Cannot call non function value `{fun:?}`"),
        }
    }

    #[track_caller]
    #[debug_requires(closure.arity() == args.len())]
    #[debug_requires(closure.is_closed((), self.meta_env.len()))]
    pub fn apply_fun_closure(&self, closure: &FunClosure, args: Vec<Arc<Value>>) -> Arc<Value> {
        let mut env = closure.env.clone();
        let mut eval_ctx = self.eval_ctx(&mut env);
        eval_ctx.push_telescope_defs(&closure.args, args);
        eval_ctx.eval_expr(&closure.body)
    }

    #[track_caller]
    #[debug_requires(scrut.is_closed(self.local_env, self.meta_env.len()))]
    #[debug_requires(closure.is_closed((), self.meta_env.len()))]
    #[debug_ensures(ret.is_closed(self.local_env, self.meta_env.len()))]
    pub fn apply_match_closure(&self, scrut: Arc<Value>, closure: MatchClosure) -> Arc<Value> {
        let mut scrut = scrut;
        if let Value::Stuck(_, spine) = Arc::make_mut(&mut scrut) {
            spine.push(Elim::Match(closure));
            return scrut;
        };

        let MatchClosure { mut env, branches } = closure;
        let mut eval_ctx = self.eval_ctx(&mut env);
        for (pat, expr) in branches.iter() {
            match pat {
                Pat::Error => {
                    eval_ctx.push_pat_defs(pat, scrut);
                    return eval_ctx.eval_expr(expr);
                }
                Pat::Name(_) => {
                    eval_ctx.push_pat_defs(pat, scrut);
                    return eval_ctx.eval_expr(expr);
                }
                Pat::Lit(lit1) if scrut.as_ref() == &Value::Lit(lit1.clone()) => {
                    eval_ctx.push_pat_defs(pat, scrut);
                    return eval_ctx.eval_expr(expr);
                }
                Pat::Lit(_) => continue,
                Pat::Variant(..) => todo!(),
            }
        }

        unreachable!("non-exhaustive match: {scrut:?}")
    }

    #[track_caller]
    #[debug_requires(closure.is_closed((), self.meta_env.len()))]
    pub fn split_fun_closure(
        &self,
        mut closure: FunClosure,
    ) -> Option<(FunArg<Arc<Value>>, impl FnOnce(Arc<Value>) -> FunClosure)> {
        let (FunArg { pat, r#type }, args) = closure.args.0.split_first()?;
        let pat = pat.clone();
        let args = Arc::from(args);
        let r#type = self.eval_ctx(&mut closure.env).eval_expr(r#type);

        let arg = FunArg {
            pat: pat.clone(),
            r#type,
        };
        let cont = move |prev| {
            closure.env.push_pat_defs(&pat, prev);
            closure.args = Telescope(args);
            closure
        };
        Some((arg, cont))
    }

    #[track_caller]
    #[debug_requires(closure.is_closed((), self.meta_env.len()))]
    pub fn split_match_closure(
        &self,
        mut closure: MatchClosure,
    ) -> Option<((Pat, Arc<Value>), impl FnOnce(Arc<Value>) -> MatchClosure)> {
        let ((pat, expr), rest) = closure.branches.split_first()?;
        let pat = pat.clone();
        let rest = Arc::from(rest);
        let value = self.eval_ctx(&mut closure.env).eval_expr(expr);
        let first = (pat.clone(), value);

        let cont = move |prev| {
            closure.env.push_pat_defs(&pat, prev);
            closure.branches = rest;
            closure
        };
        Some((first, cont))
    }
}

pub struct QuoteCtx<'env> {
    local_env: EnvLen,
    meta_env: &'env MetaEnv,
    db: &'env dyn crate::Db,
    flags: EvalFlags,
}

impl<'env> QuoteCtx<'env> {
    pub fn new(
        local_env: EnvLen,
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

    fn elim_ctx(&self) -> ElimCtx {
        ElimCtx::new(self.local_env, self.meta_env, self.db, self.flags)
    }

    #[track_caller]
    #[debug_requires(value.is_closed(self.local_env, self.meta_env.len()))]
    #[debug_ensures(ret.is_closed(self.local_env, self.meta_env.len()))]
    #[debug_ensures(self.local_env == old(self.local_env))]
    pub fn quote_value(&mut self, value: &Value) -> Expr {
        match value {
            Value::Lit(lit) => Expr::Lit(lit.clone()),
            Value::Stuck(head, spine) => {
                let head_expr = match head {
                    Head::Local(var) => match self.local_env.level_to_index(*var) {
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
                    Elim::Match(closure) => {
                        let mut closure = closure.clone();
                        let mut branches = Vec::with_capacity(closure.branches.len());
                        while let Some(((pat, value), cont)) =
                            self.elim_ctx().split_match_closure(closure.clone())
                        {
                            branches.push((pat, self.quote_value(&value)));
                            closure = cont(value);
                        }
                        Expr::Match(Arc::new(head_core), Arc::from(branches))
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

    #[track_caller]
    #[debug_requires(closure.is_closed((), self.meta_env.len()))]
    #[debug_ensures(ret.0.is_closed(self.local_env, self.meta_env.len()))]
    #[debug_ensures(self.local_env == old(self.local_env))]
    fn quote_closure(&mut self, closure: &FunClosure) -> (Telescope<Expr>, Expr) {
        let start_len = self.local_env;

        let initial_closure = closure.clone();
        let mut closure = closure.clone();
        let mut fun_args = Vec::with_capacity(closure.arity());
        let mut arg_values = Vec::with_capacity(closure.arity());

        while let Some((fun_arg, cont)) = self.elim_ctx().split_fun_closure(closure.clone()) {
            let arg_value = Arc::new(Value::local(self.local_env.to_level()));
            closure = cont(arg_value.clone());
            fun_args.push(self.quote_fun_arg(fun_arg));
            arg_values.push(arg_value);
        }

        let body = self
            .elim_ctx()
            .apply_fun_closure(&initial_closure, arg_values);
        let body = self.quote_value(&body);
        self.local_env.truncate(start_len);

        (Telescope(Arc::from(fun_args)), body)
    }

    fn quote_fun_arg(&mut self, arg: FunArg<Arc<Value>>) -> FunArg<Expr> {
        let FunArg { pat, r#type } = arg;
        let r#type = self.quote_value(&r#type);
        self.local_env.push_pat_params(&pat);
        FunArg { pat, r#type }
    }
}
