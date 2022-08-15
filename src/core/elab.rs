use std::rc::Rc;

use text_size::TextRange;

use super::conv::ConvCtx;
use super::env::LocalEnv;
use super::errors::Error;
use super::eval::{ElimCtx, EvalCtx};
use super::quote::QuoteCtx;
use super::{Expr, FunClosure, Pat, Value};
use crate::surface;

pub struct ElabCtx {
    pub local_env: LocalEnv,
    pub errors: Vec<Error>,
}

impl ElabCtx {
    pub fn new() -> Self {
        Self {
            local_env: LocalEnv::new(),
            errors: Vec::new(),
        }
    }

    pub fn eval_ctx(&mut self) -> EvalCtx { EvalCtx::new(&mut self.local_env.values) }

    pub fn elim_ctx(&self) -> ElimCtx { ElimCtx::new() }

    pub fn quote_ctx(&self) -> QuoteCtx { QuoteCtx::new(self.local_env.values.len()) }

    pub fn conv_ctx(&self) -> ConvCtx { ConvCtx::new(self.local_env.values.len()) }
}

impl ElabCtx {
    pub fn synth_expr(&mut self, expr: &surface::Expr<TextRange>) -> (Expr, Rc<Value>) {
        match expr {
            surface::Expr::Error(_) => (Expr::Error, Rc::new(Value::Error)),
            surface::Expr::Name(_, name) => {
                if let Some((idx, ty)) = self.local_env.lookup(name) {
                    return (Expr::Local(idx), ty);
                }

                match name.as_ref() {
                    "Type" => return (Expr::Type, Rc::new(Value::Type)),
                    "Bool" => return (Expr::BoolType, Rc::new(Value::Type)),
                    _ => {}
                }

                todo!("Unbound name: {name}");
            }
            surface::Expr::Bool(_, b) => (Expr::Bool(*b), Rc::new(Value::BoolType)),
            surface::Expr::FunType(_, pats, ret) => {
                let arg_cores = pats
                    .iter()
                    .map(|pat| {
                        let (_, pat_type) = self.synth_pat(pat);
                        let pat_type_core = self.quote_ctx().quote_value(&pat_type);
                        let pat_name = pat.name();
                        self.local_env.push_param(pat_name, pat_type);
                        pat_type_core
                    })
                    .collect();
                let ret_core = self.check_expr_is_type(ret);
                (
                    Expr::FunType(arg_cores, Rc::new(ret_core)),
                    Rc::new(Value::Type),
                )
            }
            surface::Expr::FunExpr(_, pats, body) => {
                let mut arg_type_cores = Vec::with_capacity(pats.len());
                let mut arg_types = Vec::with_capacity(pats.len());

                for pat in pats.iter() {
                    let name = pat.name();
                    let (_, pat_type) = self.synth_pat(pat);
                    let pat_type_core = self.quote_ctx().quote_value(&pat_type);
                    self.local_env.push_param(name, pat_type.clone());
                    arg_type_cores.push(pat_type_core);
                    arg_types.push(pat_type);
                }

                let (body_core, body_type) = self.synth_expr(body);
                let body_type_core = self.quote_ctx().quote_value(&body_type);

                let arg_type_cores: Rc<[_]> = Rc::from(arg_type_cores);
                let fun_expr = Expr::FunExpr(arg_type_cores.clone(), Rc::new(body_core));
                let fun_type = Rc::new(Value::FunType(FunClosure::new(
                    self.local_env.values.clone(),
                    arg_type_cores,
                    Rc::new(body_type_core),
                )));
                (fun_expr, fun_type)
            }
            surface::Expr::FunCall(_, fun, args) => {
                let (fun_core, fun_type) = self.synth_expr(fun);
                let closure = match fun_type.as_ref() {
                    Value::FunType(closure) => closure,
                    _ => todo!("tried to call non-fn"),
                };

                if args.len() != closure.arity() {
                    todo!("arity mismatch")
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
                self.local_env.push_param(name, pat_type);
                let (body_core, body_type) = self.synth_expr(body);
                (Expr::Let(Rc::new(init_core), Rc::new(body_core)), body_type)
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

    pub fn check_expr(&mut self, expr: &surface::Expr<TextRange>, expected: &Rc<Value>) -> Expr {
        match (expr, expected.as_ref()) {
            (surface::Expr::FunExpr(_, pats, body), Value::FunType(closure)) => {
                if pats.len() != closure.arity() {
                    todo!("arity mismatch")
                }

                let initial_closure = closure.clone();
                let mut closure = closure.clone();

                let mut args_values = Vec::with_capacity(pats.len());
                let mut arg_types = Vec::with_capacity(pats.len());
                let mut pats = pats.iter();

                while let Some((pat, (expected, cont))) =
                    Option::zip(pats.next(), self.elim_ctx().split_fun_closure(closure))
                {
                    let pat_name = pat.name();
                    let _ = self.check_pat(pat, &expected);
                    let arg_type = self.quote_ctx().quote_value(&expected);
                    arg_types.push(arg_type);

                    let arg_value = self.local_env.push_param(pat_name, expected.clone());
                    closure = cont(arg_value.clone());
                    args_values.push(arg_value);
                }

                let expected_ret = self.elim_ctx().call_closure(&initial_closure, args_values);
                let ret_core = self.check_expr(body, &expected_ret);

                Expr::FunExpr(Rc::from(arg_types), Rc::new(ret_core))
            }
            _ => {
                let (core, got) = self.synth_expr(expr);
                if self.conv_ctx().conv_values(&got, expected) {
                    core
                } else {
                    todo!("Type mismatch: expected {expected:?}, got {got:?}")
                }
            }
        }
    }

    pub fn synth_pat(&mut self, pat: &surface::Pat<TextRange>) -> (Pat, Rc<Value>) {
        match pat {
            surface::Pat::Error(_) => todo!("needs unification"),
            surface::Pat::Wildcard(_) => todo!("needs unification"),
            surface::Pat::Name(..) => todo!("needs unification"),
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
                if self.conv_ctx().conv_values(&got, expected) {
                    core
                } else {
                    todo!("Type mismatch: expected {expected:?}, got {got:?}")
                }
            }
        }
    }
}
