use std::sync::Arc;

use super::env::{LocalEnv, MetaEnv, MetaSource, NameSource};
use super::eval::{ElimCtx, EvalCtx};
use super::quote::QuoteCtx;
use super::syntax::*;
use super::unify::{PartialRenaming, UnifyCtx};
use crate::surface::syntax as surface;

pub struct ElabCtx {
    local_env: LocalEnv,
    meta_env: MetaEnv,
    renaming: PartialRenaming,
    name_source: NameSource,
}

impl ElabCtx {
    pub fn new() -> Self {
        Self {
            local_env: LocalEnv::new(),
            meta_env: MetaEnv::new(),
            renaming: PartialRenaming::default(),
            name_source: NameSource::default(),
        }
    }
}

#[salsa::tracked(return_ref)]
pub fn elab_let_def(db: &dyn crate::Db, source: surface::LetDef) -> (Expr, Expr) {
    let mut ctx = ElabCtx::new();
    let type_expr = source.ty(db);
    let body_expr = source.expr(db);

    let type_core = match type_expr {
        Some(type_expr) => {
            let CheckExpr(expr) = ctx.check_expr_is_type(type_expr);
            expr
        }
        None => {
            let name = ctx.name_source.fresh();
            let source = MetaSource::LetItemType;
            ctx.push_meta_expr(name, source, Arc::new(Value::Type))
        }
    };
    let type_value = ctx.eval_ctx().eval_expr(&type_core);

    let CheckExpr(body_core) = ctx.check_expr(body_expr, &type_value);
    let body_value = ctx.eval_ctx().eval_expr(&body_core);
    let forced_body_value = ctx.elim_ctx().force_value(&body_value);
    let forced_body_expr = ctx.quote_ctx().quote_value(&forced_body_value);

    let forced_type_value = ctx.elim_ctx().force_value(&type_value);
    let forced_type_core = ctx.quote_ctx().quote_value(&forced_type_value);

    (forced_body_expr, forced_type_core)
}

pub struct SynthExpr(Expr, Arc<Value>);

pub struct CheckExpr(Expr);

impl ElabCtx {
    pub fn eval_ctx(&mut self) -> EvalCtx {
        EvalCtx::new(&mut self.local_env.values, &self.meta_env.values)
    }

    pub fn elim_ctx(&self) -> ElimCtx { ElimCtx::new(&self.meta_env.values) }

    pub fn quote_ctx(&mut self) -> QuoteCtx {
        QuoteCtx::new(self.local_env.values.len(), &self.meta_env.values)
    }

    pub fn unify_ctx(&mut self) -> UnifyCtx {
        UnifyCtx::new(
            self.local_env.values.len(),
            &mut self.meta_env.values,
            &mut self.renaming,
        )
    }
}

impl ElabCtx {
    fn push_meta_expr(&mut self, name: VarName, source: MetaSource, ty: Arc<Value>) -> Expr {
        let var = self.meta_env.push(name, source, ty);
        Expr::MetaInsertion(var, self.local_env.sources.clone())
    }

    fn push_meta_value(&mut self, name: VarName, source: MetaSource, ty: Arc<Value>) -> Arc<Value> {
        let expr = self.push_meta_expr(name, source, ty);
        self.eval_ctx().eval_expr(&expr)
    }

    fn synth_lit(&mut self, lit: &surface::Lit) -> (Lit, Arc<Value>) {
        match lit {
            surface::Lit::Bool(b) => (Lit::Bool(*b), Arc::new(Value::BoolType)),
        }
    }

    fn synth_expr(&mut self, expr: &surface::Expr) -> SynthExpr {
        match expr {
            surface::Expr::Error => {
                let name = self.name_source.fresh();
                let source = MetaSource::Error;
                let ty = self.push_meta_value(name, source, Arc::new(Value::Type));
                SynthExpr(Expr::Error, ty)
            }
            surface::Expr::Paren(expr) => self.synth_expr(expr),
            surface::Expr::Lit(lit) => {
                let (lit, ty) = self.synth_lit(lit);
                SynthExpr(Expr::Lit(lit), ty)
            }
            surface::Expr::Name(name) => {
                if let Some((index, ty)) = self.local_env.lookup(name) {
                    return SynthExpr(Expr::Local(index), ty);
                }

                match name.as_str() {
                    "Type" => return SynthExpr(Expr::Type, Arc::new(Value::Type)),
                    "Bool" => return SynthExpr(Expr::BoolType, Arc::new(Value::Type)),
                    _ => {}
                }

                todo!("Unbound variable: `{name}`")
            }
            surface::Expr::FunType(pats, ret) => {
                let initial_len = self.local_env.len();
                let args: Vec<_> = pats
                    .iter()
                    .map(|pat| {
                        let (pat_core, pat_type) = self.synth_ann_pat(pat);
                        let pat_name = pat.pat.name();
                        let type_core = self.quote_ctx().quote_value(&pat_type);
                        self.local_env.push_param(pat_name, pat_type);
                        type_core
                    })
                    .collect();
                let CheckExpr(ret) = self.check_expr_is_type(ret);
                self.local_env.truncate(initial_len);
                SynthExpr(
                    Expr::FunType(Arc::from(args), Arc::new(ret)),
                    Arc::new(Value::Type),
                )
            }
            surface::Expr::FunExpr(pats, body) => {
                let initial_len = self.local_env.len();
                let args: Vec<_> = pats
                    .iter()
                    .map(|pat| {
                        let (pat_core, pat_type) = self.synth_ann_pat(pat);
                        let pat_name = pat.pat.name();
                        let type_core = self.quote_ctx().quote_value(&pat_type);
                        self.local_env.push_param(pat_name, pat_type);
                        type_core
                    })
                    .collect();
                let args: Arc<[_]> = Arc::from(args);

                let SynthExpr(body_core, body_type) = self.synth_expr(body);
                let ret_type = self.quote_ctx().quote_value(&body_type);
                self.local_env.truncate(initial_len);

                let fun_core = Expr::FunExpr(args.clone(), Arc::new(body_core));
                let closure =
                    FunClosure::new(self.local_env.values.clone(), args, Arc::new(ret_type));
                let fun_type = Value::FunType(closure);
                SynthExpr(fun_core, Arc::new(fun_type))
            }
            surface::Expr::FunCall(fun, args) => {
                let SynthExpr(fun_core, fun_type) = self.synth_expr(fun);
                let fun_type = self.elim_ctx().force_value(&fun_type);
                let closure = match fun_type.as_ref() {
                    Value::FunType(closure) => closure,
                    Value::Error => todo!(),
                    _ => todo!(),
                };

                let expected_arity = closure.arity();
                let actual_arity = args.len();
                if actual_arity != expected_arity {
                    todo!()
                }

                let initial_closure = closure.clone();
                let mut closure = closure.clone();

                let mut arg_cores = Vec::with_capacity(args.len());
                let mut arg_values = Vec::with_capacity(args.len());
                let mut args = args.iter();

                while let Some((arg, (expected, cont))) =
                    Option::zip(args.next(), self.elim_ctx().split_fun_closure(closure))
                {
                    let CheckExpr(arg_core) = self.check_expr(arg, &expected);
                    let arg_value = self.eval_ctx().eval_expr(&arg_core);
                    closure = cont(arg_value.clone());
                    arg_cores.push(arg_core);
                    arg_values.push(arg_value);
                }

                let ret_type = self.elim_ctx().apply_closure(&initial_closure, arg_values);
                SynthExpr(
                    Expr::FunCall(Arc::new(fun_core), Arc::from(arg_cores)),
                    ret_type,
                )
            }
            surface::Expr::Let(pat, init, body) => {
                let (pat_core, type_value) = self.synth_ann_pat(pat);
                let pat_name = pat.pat.name();
                let type_core = self.quote_ctx().quote_value(&type_value);

                let CheckExpr(init_core) = self.check_expr(init, &type_value);
                let init_value = self.eval_ctx().eval_expr(&init_core);

                self.local_env.push_def(pat_name, init_value, type_value);
                let SynthExpr(body_core, body_type) = self.synth_expr(body);
                self.local_env.pop();

                SynthExpr(
                    Expr::Let(
                        Arc::new(pat_core),
                        Arc::new(type_core),
                        Arc::new(init_core),
                        Arc::new(body_core),
                    ),
                    body_type,
                )
            }
            surface::Expr::Match(..) => todo!(),
        }
    }

    fn check_expr_is_type(&mut self, expr: &surface::Expr) -> CheckExpr {
        self.check_expr(expr, &Arc::new(Value::Type))
    }

    fn check_expr(&mut self, expr: &surface::Expr, expected: &Arc<Value>) -> CheckExpr {
        match (expr, expected.as_ref()) {
            (surface::Expr::Error, _) => CheckExpr(Expr::Error),
            (surface::Expr::FunExpr(pats, body), Value::FunType(closure))
                if pats.len() == closure.arity() =>
            {
                let initial_len = self.local_env.len();
                let initial_closure = closure.clone();
                let mut closure = closure.clone();

                let mut args_values = Vec::with_capacity(pats.len());
                let mut arg_types = Vec::with_capacity(pats.len());

                let mut pats = pats.iter();
                while let Some((pat, (expected, cont))) =
                    Option::zip(pats.next(), self.elim_ctx().split_fun_closure(closure))
                {
                    let type_core = self.quote_ctx().quote_value(&expected);

                    let arg_value = Arc::new(Value::local(self.local_env.len().to_level()));
                    let pat_core = self.check_ann_pat(pat, &expected);
                    let pat_name = pat.pat.name();
                    self.local_env.push_param(pat_name.clone(), expected);

                    closure = cont(arg_value.clone());
                    args_values.push(arg_value);
                    arg_types.push(type_core);
                }

                let expected_ret = self.elim_ctx().apply_closure(&initial_closure, args_values);
                let CheckExpr(ret_core) = self.check_expr(body, &expected_ret);
                self.local_env.truncate(initial_len);

                CheckExpr(Expr::FunExpr(Arc::from(arg_types), Arc::new(ret_core)))
            }
            (surface::Expr::Let(pat, init, body), _) => {
                let (pat_core, type_value) = self.synth_ann_pat(pat);
                let pat_name = pat.pat.name();
                let type_core = self.quote_ctx().quote_value(&type_value);

                let CheckExpr(init_core) = self.check_expr(init, &type_value);
                let init_value = self.eval_ctx().eval_expr(&init_core);

                self.local_env.push_def(pat_name, init_value, type_value);
                let CheckExpr(body_core) = self.check_expr(body, expected);
                self.local_env.pop();

                CheckExpr(Expr::Let(
                    Arc::new(pat_core),
                    Arc::new(type_core),
                    Arc::new(init_core),
                    Arc::new(body_core),
                ))
            }
            _ => {
                let SynthExpr(core, got) = self.synth_expr(expr);
                match self.unify_ctx().unify_values(&got, expected) {
                    Ok(()) => CheckExpr(core),
                    Err(error) => todo!(),
                }
            }
        }
    }

    fn synth_ann_pat(&mut self, pat: &surface::AnnPat) -> (Pat, Arc<Value>) {
        let surface::AnnPat { pat, ty } = pat;
        match ty {
            None => self.synth_pat(pat),
            Some(ty) => {
                let CheckExpr(type_core) = self.check_expr_is_type(ty);
                let type_value = self.eval_ctx().eval_expr(&type_core);
                let pat_core = self.check_pat(pat, &type_value);
                (pat_core, type_value)
            }
        }
    }

    fn check_ann_pat(&mut self, pat: &surface::AnnPat, expected: &Arc<Value>) -> Pat {
        let surface::AnnPat { pat, ty } = pat;
        match ty {
            None => self.check_pat(pat, expected),
            Some(ty) => {
                let CheckExpr(type_core) = self.check_expr_is_type(ty);
                let type_value = self.eval_ctx().eval_expr(&type_core);
                self.check_pat(pat, expected)
            }
        }
    }

    fn synth_pat(&mut self, pat: &surface::Pat) -> (Pat, Arc<Value>) {
        let mut meta = || {
            let name = self.name_source.fresh();
            let source = MetaSource::PatType;
            self.push_meta_value(name, source, Arc::new(Value::Type))
        };
        match pat {
            surface::Pat::Paren(pat) => self.synth_pat(pat),
            surface::Pat::Error => (Pat::Error, meta()),
            surface::Pat::Wildcard => (Pat::Name(VarName::Underscore), meta()),
            surface::Pat::Name(name) => (Pat::Name(VarName::User(name.clone())), meta()),
            surface::Pat::Lit(lit) => {
                let (lit, ty) = self.synth_lit(lit);
                (Pat::Lit(lit), ty)
            }
        }
    }

    fn check_pat(&mut self, pat: &surface::Pat, expected: &Arc<Value>) -> Pat {
        match pat {
            surface::Pat::Paren(pat) => self.check_pat(pat, expected),
            surface::Pat::Error => Pat::Error,
            surface::Pat::Wildcard => Pat::Name(VarName::Underscore),
            surface::Pat::Name(name) => Pat::Name(VarName::User(name.clone())),
            _ => {
                let (core, got) = self.synth_pat(pat);
                match self.unify_ctx().unify_values(&got, expected) {
                    Ok(()) => core,
                    Err(error) => todo!(),
                }
            }
        }
    }
}
