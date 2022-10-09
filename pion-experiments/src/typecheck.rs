use std::rc::Rc;

use contracts::{debug_ensures, debug_requires};
use text_size::TextRange;

use crate::env::LocalEnv;
use crate::semantics::{reduce, Const, EvalFlags, Expr, RcValue, Value};
use crate::surface;

pub struct Ctx {
    pub local_env: LocalEnv,
}

impl Ctx {
    pub fn with_param(&self, name: String, r#type: RcValue) -> Self {
        let value = Rc::new(Value::Local(self.local_env.len().to_level()));
        Self {
            local_env: self.local_env.pushed(name, r#type, value),
        }
    }

    pub fn eval(&self, expr: &Expr) -> RcValue {
        reduce(EvalFlags::EVAL_WHNF, &self.local_env.values, expr)
    }
}

pub type SynthExpr = (Expr, RcValue);
pub type CheckExpr = Expr;

#[debug_ensures(ret.0.is_closed(ctx.local_env.len()))]
#[debug_ensures(ret.1.is_closed(ctx.local_env.len()))]
pub fn synth_expr(ctx: &Ctx, surface: &surface::Expr<TextRange>) -> SynthExpr {
    match surface {
        surface::Expr::Name(_, name) => {
            if let Some((var, r#type, _)) = ctx.local_env.lookup(name) {
                return (Expr::Local(var), r#type.clone());
            }

            match name.as_str() {
                "Type" => return (Expr::TYPE, Rc::new(Value::TYPE)),
                "String" => return (Expr::Const(Const::StringType), Rc::new(Value::TYPE)),
                _ => {}
            }

            todo!("Unbound local variable")
        }
        surface::Expr::App(_, fun, arg) => {
            let (fun_expr, fun_type) = synth_expr(ctx, fun);
            let (domain, range) = match fun_type.as_ref() {
                Value::FunType(domain, range) => (domain, range),
                _ => todo!("tried to apply non function"),
            };
            let arg_expr = check_expr(ctx, arg, domain);
            let arg_value = ctx.eval(&arg_expr);
            let ret_type = range.apply(EvalFlags::EVAL_WHNF, arg_value);
            (fun_expr, ret_type)
        }
        surface::Expr::FunType(_, name, domain, range) => {
            let r#type = Rc::new(Value::TYPE);
            let domain_expr = check_expr(ctx, domain, &r#type);
            let domain_type = ctx.eval(&domain_expr);
            let range_expr = check_expr(&ctx.with_param(name.clone(), domain_type), range, &r#type);
            (
                Expr::FunType(Rc::new(domain_expr), Rc::new(range_expr)),
                r#type,
            )
        }
        surface::Expr::FunExpr(..) => todo!("Cannot check fun exprs without unification"),
    }
}

#[debug_requires(expected.is_closed(ctx.local_env.len()))]
#[debug_ensures(ret.is_closed(ctx.local_env.len()))]
pub fn check_expr(ctx: &Ctx, surface: &surface::Expr<TextRange>, expected: &RcValue) -> CheckExpr {
    match (surface, expected.as_ref()) {
        (
            surface::Expr::FunExpr(_, name, body),
            Value::FunType(expected_domain, expected_range),
        ) => {
            let arg_value = Rc::new(Value::Local(ctx.local_env.len().to_level()));
            let expected_value = &expected_range.apply(EvalFlags::EVAL_WHNF, arg_value);
            let body_core = check_expr(
                &ctx.with_param(name.clone(), expected_domain.clone()),
                body,
                expected_value,
            );
            Expr::FunExpr(Rc::new(body_core))
        }
        _ => todo!(),
    }
}
