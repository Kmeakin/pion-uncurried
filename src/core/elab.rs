use std::rc::Rc;

use text_size::TextRange;

use super::conv::ConvCtx;
use super::env::LocalEnv;
use super::errors::Error;
use super::eval::{ElimCtx, EvalCtx};
use super::quote::QuoteCtx;
use super::{Expr, FunClosure, Pat, Telescope, Value};
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
                let mut names = Vec::with_capacity(pats.len());
                let initial_len = self.local_env.len();
                let args = pats
                    .iter()
                    .map(|pat| {
                        let name = pat.name();
                        let (_, type_value) = self.synth_pat(pat);
                        let type_core = self.quote_ctx().quote_value(&type_value);
                        names.push(name.clone());
                        self.local_env.push_param(name, type_value);
                        type_core
                    })
                    .collect();
                let ret_core = self.check_expr_is_type(ret);
                self.local_env.truncate(initial_len);
                (
                    Expr::FunType(Rc::from(names), args, Rc::new(ret_core)),
                    Rc::new(Value::Type),
                )
            }
            surface::Expr::FunExpr(_, pats, body) => {
                let initial_len = self.local_env.len();

                let mut names = Vec::with_capacity(pats.len());
                let mut arg_cores = Vec::with_capacity(pats.len());
                let mut arg_types = Vec::with_capacity(pats.len());

                for pat in pats.iter() {
                    let name = pat.name();
                    let (_, type_value) = self.synth_pat(pat);
                    let type_core = self.quote_ctx().quote_value(&type_value);
                    self.local_env.push_param(name.clone(), type_value.clone());

                    names.push(name);
                    arg_cores.push(type_core);
                    arg_types.push(type_value);
                }

                let names: Rc<[_]> = Rc::from(names);
                let arg_cores: Rc<[_]> = Rc::from(arg_cores);
                let arg_types: Rc<[_]> = Rc::from(arg_types);

                let (body_core, body_type) = self.synth_expr(body);
                let body_type = self.quote_ctx().quote_value(&body_type);
                self.local_env.truncate(initial_len);

                let telescope = Telescope::new(names.clone(), arg_types);
                let closure = FunClosure::new(
                    self.local_env.values.clone(),
                    pats.len(),
                    Rc::new(body_type),
                );

                (
                    Expr::FunExpr(Rc::from(names), arg_cores, Rc::new(body_core)),
                    Rc::new(Value::FunType(telescope, closure)),
                )
            }
            surface::Expr::FunCall(_, fun, args) => {
                let (fun_core, fun_type) = self.synth_expr(fun);
                let (telescope, ret) = match fun_type.as_ref() {
                    Value::FunType(telescope, ret) => (telescope, ret),
                    _ => todo!("tried to call non-fn"),
                };

                if args.len() != telescope.arity() {
                    todo!("arity mismatch")
                }

                let mut arg_cores = Vec::with_capacity(args.len());
                let mut arg_values = Vec::with_capacity(args.len());

                for (actual_arg, expected_arg) in args.iter().zip(telescope.types.iter()) {
                    let arg_core = self.check_expr(actual_arg, expected_arg);
                    let arg_value = self.eval_ctx().eval_expr(&arg_core);
                    arg_cores.push(arg_core);
                    arg_values.push(arg_value);
                }

                let ret = self.elim_ctx().call_closure(ret, arg_values);

                (Expr::FunCall(Rc::new(fun_core), Rc::from(arg_cores)), ret)
            }
            surface::Expr::Let(_, pat, init, body) => {
                let name = pat.name();
                let (init_core, init_type) = self.synth_expr(init);
                self.check_pat(pat, &init_type);
                self.local_env.push_param(name.clone(), init_type);
                let (body_core, body_type) = self.synth_expr(body);
                (
                    Expr::Let(name.clone(), Rc::new(init_core), Rc::new(body_core)),
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

    pub fn check_expr(&mut self, expr: &surface::Expr<TextRange>, expected: &Rc<Value>) -> Expr {
        match (expr, expected.as_ref()) {
            (surface::Expr::FunExpr(_, args, body), Value::FunType(telescope, ret)) => {
                let mut names = Vec::with_capacity(args.len());
                let mut arg_types = Vec::with_capacity(args.len());
                let mut arg_values = Vec::with_capacity(args.len());
                let initial_len = self.local_env.len();

                for (pat, expected) in args.iter().zip(telescope.types.iter()) {
                    let name = pat.name();
                    let pat_core = self.check_pat(pat, expected);
                    let arg_type = self.quote_ctx().quote_value(expected);

                    let arg_value = self.local_env.push_param(name.clone(), expected.clone());
                    names.push(name);
                    arg_types.push(arg_type);
                    arg_values.push(arg_value);
                }

                let ret = self.elim_ctx().call_closure(ret, arg_values);
                let body_core = self.check_expr(body, &ret);

                self.local_env.truncate(initial_len);

                Expr::FunExpr(Rc::from(names), Rc::from(arg_types), Rc::new(body_core))
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
            surface::Pat::Ann(_, pat, ty) => todo!(),
        }
    }
}
