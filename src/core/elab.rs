use std::rc::Rc;

use contracts::debug_ensures;
use text_size::TextRange;

use super::conv::ConvCtx;
use super::env::{LocalEnv, MetaEnv};
use super::errors::ElabError;
use super::eval::{ElimCtx, EvalCtx};
use super::quote::QuoteCtx;
use super::unelab::UnelabCtx;
use super::unify::{PartialRenaming, UnifyCtx};
use super::{Expr, FunClosure, MetaSource, Pat, Value};
use crate::surface;
use crate::surface::pretty::PrettyCtx;

pub struct ElabCtx {
    pub local_env: LocalEnv,
    pub meta_env: MetaEnv,
    pub renaming: PartialRenaming,
    pub errors: Vec<ElabError>,
}

impl ElabCtx {
    pub fn new() -> Self {
        Self {
            local_env: LocalEnv::new(),
            meta_env: MetaEnv::new(),
            renaming: PartialRenaming::new(),
            errors: Vec::new(),
        }
    }

    pub fn drain_errors(&mut self) -> impl Iterator<Item = ElabError> + '_ {
        self.errors.drain(..).chain(self.meta_env.errors())
    }

    pub fn eval_ctx(&mut self) -> EvalCtx {
        EvalCtx::new(&mut self.local_env.values, &self.meta_env.values)
    }

    pub fn elim_ctx(&self) -> ElimCtx { ElimCtx::new(&self.meta_env.values) }

    pub fn quote_ctx(&self) -> QuoteCtx {
        QuoteCtx::new(self.local_env.values.len(), &self.meta_env.values)
    }

    pub fn conv_ctx(&self) -> ConvCtx {
        ConvCtx::new(self.local_env.values.len(), &self.meta_env.values)
    }

    pub fn unelab_ctx(&mut self) -> UnelabCtx<'_> { UnelabCtx::new(&mut self.local_env.names) }

    pub fn pretty_ctx(&self) -> PrettyCtx { PrettyCtx::new() }

    pub fn pretty_surface_expr<Range>(&mut self, expr: &surface::Expr<Range>) -> String {
        let pretty_ctx = self.pretty_ctx();
        let doc = pretty_ctx.pretty_expr(expr).into_doc();
        doc.pretty(80).to_string()
    }

    pub fn pretty_core_expr(&mut self, expr: &Expr) -> String {
        let surface = self.unelab_ctx().unelab_expr(expr);
        self.pretty_surface_expr(&surface)
    }

    pub fn pretty_value(&mut self, value: &Rc<Value>) -> String {
        let core = self.quote_ctx().quote_value(value);
        self.pretty_core_expr(&core)
    }

    fn with_scope<T>(&mut self, mut f: impl FnMut(&mut Self) -> T) -> T {
        let initial_len = self.local_env.len();
        let ret = f(self);
        self.local_env.truncate(initial_len);
        ret
    }

    fn push_meta_expr(&mut self, source: MetaSource, ty: Rc<Value>) -> Expr {
        let var = self.meta_env.push(source, ty);
        Expr::MetaInsertion(var, self.local_env.infos.clone())
    }

    fn push_meta_value(&mut self, source: MetaSource, ty: Rc<Value>) -> Rc<Value> {
        let expr = self.push_meta_expr(source, ty);
        self.eval_ctx().eval_expr(&expr)
    }

    fn unify_ctx(&mut self) -> UnifyCtx {
        UnifyCtx::new(
            &mut self.renaming,
            self.local_env.values.len(),
            &mut self.meta_env.values,
        )
    }
}

impl ElabCtx {
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    pub fn synth_expr(&mut self, expr: &surface::Expr<TextRange>) -> (Expr, Rc<Value>) {
        self.synth_expr_inner(expr)
    }

    fn synth_error_expr(&mut self) -> (Expr, Rc<Value>) {
        let ty = self.push_meta_value(MetaSource::Error, Rc::new(Value::Type));
        (Expr::Error, ty)
    }

    fn synth_expr_inner(&mut self, expr: &surface::Expr<TextRange>) -> (Expr, Rc<Value>) {
        match expr {
            surface::Expr::Error(_) => self.synth_error_expr(),
            surface::Expr::Placeholder(range) => {
                let type_source = MetaSource::PlaceholderType(*range);
                let expr_source = MetaSource::PlaceholderExpr(*range);
                let ty = self.push_meta_value(type_source, Rc::new(Value::Type));
                let expr = self.push_meta_expr(expr_source, ty.clone());
                (expr, ty)
            }
            surface::Expr::Name(range, name) => {
                if let Some((idx, ty)) = self.local_env.lookup(name) {
                    return (Expr::Local(idx), ty);
                }

                match name.as_ref() {
                    "Type" => return (Expr::Type, Rc::new(Value::Type)),
                    "Bool" => return (Expr::BoolType, Rc::new(Value::Type)),
                    _ => {}
                }
                self.errors.push(ElabError::UnboundName {
                    range: *range,
                    name: name.clone(),
                });
                self.synth_error_expr()
            }
            surface::Expr::Bool(_, b) => (Expr::Bool(*b), Rc::new(Value::BoolType)),
            surface::Expr::FunType(_, pats, ret) => {
                let initial_len = self.local_env.len();
                let mut idx = pats.len();
                let (names, args): (Vec<_>, Vec<_>) = pats
                    .iter()
                    .map(|pat| {
                        let name = pat.name();
                        let (_, pat_type) = self.synth_pat(pat);
                        let type_core = self.quote_ctx().quote_value(&pat_type);
                        idx -= 1;
                        self.local_env.push_param(name.clone(), pat_type, idx);
                        (name, type_core)
                    })
                    .unzip();
                let ret = self.check_expr_is_type(ret);
                self.local_env.truncate(initial_len);
                (
                    Expr::FunType(Rc::from(names), Rc::from(args), Rc::new(ret)),
                    Rc::new(Value::Type),
                )
            }
            surface::Expr::FunExpr(_, pats, body) => {
                let initial_len = self.local_env.len();
                let mut idx = pats.len();
                let (names, args): (Vec<_>, Vec<_>) = pats
                    .iter()
                    .map(|pat| {
                        let name = pat.name();
                        let (_, pat_type) = self.synth_pat(pat);
                        let type_core = self.quote_ctx().quote_value(&pat_type);
                        idx -= 1;
                        self.local_env.push_param(name.clone(), pat_type, idx);
                        (name, type_core)
                    })
                    .unzip();
                let names: Rc<[_]> = Rc::from(names);
                let args: Rc<[_]> = Rc::from(args);

                let (body_core, body_type) = self.synth_expr(body);
                let ret_type = self.quote_ctx().quote_value(&body_type);
                self.local_env.truncate(initial_len);

                let fun_core = Expr::FunExpr(names.clone(), args.clone(), Rc::new(body_core));
                let closure =
                    FunClosure::new(self.local_env.values.clone(), args, Rc::new(ret_type));
                let fun_type = Value::FunType(names, closure);
                (fun_core, Rc::new(fun_type))
            }
            surface::Expr::FunCall(range, fun, args) => {
                let (fun_core, fun_type) = self.synth_expr(fun);
                let fun_type = self.elim_ctx().force_value(&fun_type);
                let closure = match fun_type.as_ref() {
                    Value::FunType(_, closure) => closure,
                    _ => {
                        self.errors.push(ElabError::CallNonFun {
                            range: fun.range(),
                            fun_type,
                        });
                        return self.synth_error_expr();
                    }
                };

                let expected_arity = closure.arity();
                let actual_arity = args.len();
                if actual_arity != expected_arity {
                    self.errors.push(ElabError::ArityMismatch {
                        range: *range,
                        fun_type,
                        expected_arity,
                        actual_arity,
                    });
                    return self.synth_error_expr();
                }

                let initial_closure = closure.clone();
                let mut closure = closure.clone();

                let mut arg_cores = Vec::with_capacity(args.len());
                let mut arg_values = Vec::with_capacity(args.len());
                let mut args = args.iter();

                while let Some((arg, (expected, cont))) =
                    Option::zip(args.next(), self.elim_ctx().split_fun_closure(closure))
                {
                    let arg_core = self.check_expr(arg, &expected);
                    let arg_value = self.eval_ctx().eval_expr(&arg_core);
                    closure = cont(arg_value.clone());
                    arg_cores.push(arg_core);
                    arg_values.push(arg_value);
                }

                let ret_type = self.elim_ctx().call_closure(&initial_closure, arg_values);
                (
                    Expr::FunCall(Rc::new(fun_core), Rc::from(arg_cores)),
                    ret_type,
                )
            }
            surface::Expr::Let(_, pat, init, body) => {
                let name = pat.name();
                let (_, pat_type) = self.synth_pat(pat);
                let init_core = self.check_expr(init, &pat_type);
                let init_value = self.eval_ctx().eval_expr(&init_core);
                self.local_env.push_def(name.clone(), init_value, pat_type);
                let (body_core, body_type) = self.synth_expr(body);
                self.local_env.pop();
                (
                    Expr::Let(name, Rc::new(init_core), Rc::new(body_core)),
                    body_type,
                )
            }
            surface::Expr::Ann(_, expr, ty) => {
                let type_core = self.check_expr_is_type(ty);
                let type_value = self.eval_ctx().eval_expr(&type_core);
                let expr_core = self.check_expr(expr, &type_value);
                (
                    Expr::Ann(Rc::new(expr_core), Rc::new(type_core)),
                    type_value.clone(),
                )
            }
        }
    }

    pub fn check_expr_is_type(&mut self, expr: &surface::Expr<TextRange>) -> Expr {
        self.check_expr(expr, &Rc::new(Value::Type))
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    pub fn check_expr(&mut self, expr: &surface::Expr<TextRange>, expected: &Rc<Value>) -> Expr {
        self.check_expr_inner(expr, expected)
    }

    fn check_expr_inner(&mut self, expr: &surface::Expr<TextRange>, expected: &Rc<Value>) -> Expr {
        let expected = self.elim_ctx().force_value(expected);
        match (expr, expected.as_ref()) {
            (surface::Expr::FunExpr(_, pats, body), Value::FunType(_, closure)) => {
                if pats.len() != closure.arity() {
                    todo!("arity mismatch")
                }

                let initial_len = self.local_env.len();
                let initial_closure = closure.clone();
                let mut closure = closure.clone();

                let mut names = Vec::with_capacity(pats.len());
                let mut args_values = Vec::with_capacity(pats.len());
                let mut arg_types = Vec::with_capacity(pats.len());

                let mut idx = pats.len();
                let mut pats = pats.iter();
                while let Some((pat, (expected, cont))) =
                    Option::zip(pats.next(), self.elim_ctx().split_fun_closure(closure))
                {
                    let name = pat.name();
                    let _ = self.check_pat(pat, &expected);
                    let arg_type = self.quote_ctx().quote_value(&expected);
                    idx -= 1;
                    let arg_value = self
                        .local_env
                        .push_param(name.clone(), expected.clone(), idx);

                    closure = cont(arg_value.clone());
                    args_values.push(arg_value);
                    arg_types.push(arg_type);
                    names.push(name);
                }

                let expected_ret = self.elim_ctx().call_closure(&initial_closure, args_values);
                let ret_core = self.check_expr(body, &expected_ret);
                self.local_env.truncate(initial_len);

                Expr::FunExpr(Rc::from(names), Rc::from(arg_types), Rc::new(ret_core))
            }
            _ => {
                let (core, got) = self.synth_expr(expr);
                match self.unify_ctx().unify_values(&got, &expected) {
                    Ok(()) => core,
                    Err(error) => {
                        self.errors.push(ElabError::TypeMismatch {
                            range: expr.range(),
                            expected_type: expected.clone(),
                            actual_type: got,
                            error,
                        });
                        Expr::Error
                    }
                }
            }
        }
    }

    fn synth_error_pat(&mut self) -> (Pat, Rc<Value>) {
        let ty = self.push_meta_value(MetaSource::Error, Rc::new(Value::Type));
        (Pat::Error, ty)
    }

    pub fn synth_pat(&mut self, pat: &surface::Pat<TextRange>) -> (Pat, Rc<Value>) {
        match pat {
            surface::Pat::Error(_) => self.synth_error_pat(),
            surface::Pat::Wildcard(range) => {
                let ty = self.push_meta_value(MetaSource::PatType(*range), Rc::new(Value::Type));
                (Pat::Wildcard, ty)
            }
            surface::Pat::Name(range, name) => {
                let ty = self.push_meta_value(MetaSource::PatType(*range), Rc::new(Value::Type));
                (Pat::Name(name.clone()), ty)
            }
            surface::Pat::Ann(_, pat, ty) => {
                let type_core = self.check_expr_is_type(ty);
                let type_value = self.eval_ctx().eval_expr(&type_core);
                let pat_core = self.check_pat(pat, &type_value);
                (pat_core, type_value)
            }
        }
    }

    pub fn check_pat(&mut self, pat: &surface::Pat<TextRange>, expected: &Rc<Value>) -> Pat {
        match pat {
            surface::Pat::Error(_) => Pat::Error,
            surface::Pat::Wildcard(_) => Pat::Wildcard,
            surface::Pat::Name(_, name) => Pat::Name(name.clone()),
            _ => {
                let (core, got) = self.synth_pat(pat);
                match self.unify_ctx().unify_values(&got, expected) {
                    Ok(_) => core,
                    Err(error) => {
                        self.errors.push(ElabError::TypeMismatch {
                            range: pat.range(),
                            expected_type: expected.clone(),
                            actual_type: got,
                            error,
                        });
                        Pat::Error
                    }
                }
            }
        }
    }
}

impl MetaEnv {
    fn errors(&self) -> impl Iterator<Item = ElabError> + '_ {
        self.values
            .iter()
            .zip(self.sources.iter())
            .filter_map(move |it| match it {
                (Some(_), _) => None,
                (None, MetaSource::PlaceholderType(_)) => None,
                (None, MetaSource::Error) => None,
                (None, source) => Some(ElabError::UnsolvedMeta { source: *source }),
            })
    }
}
