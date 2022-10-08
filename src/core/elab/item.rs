#![allow(clippy::needless_borrow)]

use super::*;
use crate::ir::lower_file;

#[salsa::tracked]
pub fn elab_module(db: &dyn crate::Db, ir: ir::Module) -> Module {
    let file = ir.file(db);
    let module = lower_file(db, file);
    let items = module
        .items(db)
        .iter()
        .filter_map(|item| match item {
            ir::Item::Let(ir) => Some(Item::Let(elab_let_def(db, *ir))),
            ir::Item::Enum(ir) => Some(Item::Enum(elab_enum_def(db, *ir))),
            ir::Item::Variant(_) => None,
        })
        .collect();
    Module { items }
}

#[salsa::tracked]
pub fn elab_let_def(db: &dyn crate::Db, ir: ir::LetDef) -> LetDef {
    let mut ctx = ElabCtx::new(db, ir.file(db));
    let name = ir.name(ctx.db);
    let surface::LetDef { ty, body, .. } = ir.surface(ctx.db);

    let (body_expr, type_value) = match ty {
        Some(ty) => {
            let CheckExpr(type_expr) = ctx.check_expr_is_type(&ty);
            let type_value = ctx.eval_ctx().eval_expr(&type_expr);
            let CheckExpr(body_expr) = ctx.check_expr(&body, &type_value);
            (body_expr, type_value)
        }
        None => {
            let SynthExpr(body_expr, type_value) = ctx.synth_expr(&body);
            (body_expr, type_value)
        }
    };
    let type_expr = ctx.quote_ctx().quote_value(&type_value);

    let expr_opts = EvalOpts {
        beta_reduce: true, // TODO: disable beta reduction
        ..EvalOpts::ZONK
    };
    let value_opts = EvalOpts {
        error_on_unsolved_meta: true,
        ..EvalOpts::EVAL_CBV
    };

    let body_value = ctx.eval_ctx().with_opts(value_opts).eval_expr(&body_expr);
    let (body_expr, _) = ctx
        .eval_ctx()
        .with_opts(expr_opts)
        .normalize_expr(&body_expr);

    let type_value = ctx.eval_ctx().with_opts(value_opts).eval_expr(&type_expr);
    let (type_expr, _) = ctx
        .eval_ctx()
        .with_opts(expr_opts)
        .normalize_expr(&type_expr);

    debug_assert!(
        body_expr.is_closed(EnvLen(0), EnvLen(0)),
        "free variables in `body_expr`: {body_expr:#?}"
    );
    debug_assert!(
        body_expr.is_closed(EnvLen(0), EnvLen(0)),
        "free variables in `type_expr`: {type_expr:#?}"
    );

    LetDef {
        name,
        body: (body_expr, body_value),
        ty: (type_expr, type_value),
    }
}

#[salsa::tracked]
/// Synthesise the type of an `Expr::EnumDef(ir)`
pub fn synth_let_def_expr(db: &dyn crate::Db, ir: ir::LetDef) -> Arc<Value> {
    let LetDef { ty, .. } = elab_let_def(db, ir);
    ty.1
}

#[salsa::tracked]
pub fn eval_let_def_expr(db: &dyn crate::Db, ir: ir::LetDef) -> Arc<Value> {
    let LetDef { body, .. } = elab_let_def(db, ir);
    ElabCtx::new(db, ir.file(db)).eval_ctx().eval_expr(&body.0)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDefSig {
    args: Arc<[FunArg<Expr>]>,
    ret_type: (Expr, Arc<Value>),
    self_type: Arc<Value>,
    subst: (LocalEnv, MetaEnv, PartialRenaming),
}

#[salsa::tracked]
/// The non recursive part of `elab_enum_def`. Crucial for "tying the knot" of
/// recursive enums.
pub fn enum_def_sig(db: &dyn crate::Db, ir: ir::EnumDef) -> EnumDefSig {
    let mut ctx = ElabCtx::new(db, ir.file(db));
    let surface::EnumDef { args, ret_type, .. } = ir.surface(db);

    let mut self_type_args = Vec::with_capacity(args.len());
    let args: Arc<[_]> = args
        .iter()
        .map(|pat| {
            let SynthPat(pat_core, type_value) = ctx.synth_ann_pat(pat);
            let type_core = ctx.quote_ctx().quote_value(&type_value);
            self_type_args.push(Arc::new(Value::local(ctx.local_env.len().to_level())));
            ctx.subst_pat(&pat_core, type_value, None);
            FunArg {
                pat: pat_core,
                ty: type_core,
            }
        })
        .collect();

    let (ret_type_expr, ret_type_value) = match ret_type {
        None => (Expr::Type, Arc::new(Value::Type)),
        Some(ret_type_surface) => {
            let SynthExpr(ret_type_expr, _) = ctx.synth_expr(&ret_type_surface);
            let ret_type_value = ctx.eval_ctx().eval_expr(&ret_type_expr);
            let expected = Arc::new(Value::Type);
            match ctx.unify_ctx().unify_values(&ret_type_value, &expected) {
                Ok(_) => (ret_type_expr, ret_type_value),
                Err(error) => {
                    ctx.report_type_mismatch(
                        ret_type_surface.span(),
                        &expected,
                        &ret_type_value,
                        error,
                    );
                    (Expr::Error, Arc::new(Value::Error))
                }
            }
        }
    };

    let self_type = match self_type_args.len() {
        0 => Arc::new(Value::enum_def(ir)),
        _ => Arc::new(Value::Stuck(
            Head::EnumDef(ir),
            vec![Elim::FunCall(self_type_args)],
        )),
    };

    EnumDefSig {
        args,
        ret_type: (ret_type_expr, ret_type_value),
        self_type,
        subst: (ctx.local_env, ctx.meta_env, ctx.renaming),
    }
}

#[salsa::tracked]
pub fn elab_enum_def(db: &dyn crate::Db, ir: ir::EnumDef) -> EnumDef {
    let surface::EnumDef { name, variants, .. } = ir.surface(db);
    let name = Symbol::new(db, name.to_owned());

    let EnumDefSig {
        args,
        ret_type,
        self_type,
        subst,
    } = enum_def_sig(db, ir);

    let mut ctx = ElabCtx::new(db, ir.file(db));
    ctx.local_env = subst.0;
    ctx.meta_env = subst.1;
    ctx.renaming = subst.2;

    let variants = variants
        .iter()
        .map(|surface| {
            let initial_len = ctx.local_env.len();
            let surface::EnumVariant {
                name,
                args,
                ret_type,
            } = surface;
            let name = Symbol::new(db, name.to_owned());

            let args = args
                .iter()
                .map(|pat| {
                    let SynthPat(pat_core, type_value) = ctx.synth_ann_pat(pat);
                    let type_core = ctx.quote_ctx().quote_value(&type_value);
                    ctx.subst_pat(&pat_core, type_value, None);
                    FunArg {
                        pat: pat_core,
                        ty: type_core,
                    }
                })
                .collect();

            let ret_type = match ret_type {
                None => (ctx.quote_ctx().quote_value(&self_type), self_type.clone()),
                Some(ret_type_surface) => {
                    let SynthExpr(ret_type_expr, _) = ctx.synth_expr(ret_type_surface);
                    let ret_type_value = ctx.eval_ctx().eval_expr(&ret_type_expr);
                    (ret_type_expr, ret_type_value)
                }
            };

            ctx.local_env.truncate(initial_len);
            EnumVariant {
                name,
                args,
                ret_type,
            }
        })
        .collect();

    ctx.report_unsolved_metas();
    EnumDef {
        name,
        args,
        ret_type,
        variants,
    }
}

#[salsa::tracked]
/// Synthesise the type of an `Expr::EnumDef(ir)`
pub fn synth_enum_def_expr(db: &dyn crate::Db, ir: ir::EnumDef) -> Arc<Value> {
    let EnumDefSig { args, ret_type, .. } = enum_def_sig(db, ir);
    match args.len() {
        0 => ret_type.1,
        _ => Arc::new(Value::FunType(FunClosure::new(
            SharedEnv::new(),
            args,
            Arc::new(ret_type.0),
        ))),
    }
}

#[salsa::tracked]
pub fn elab_enum_variant(db: &dyn crate::Db, ir: ir::EnumVariant) -> EnumVariant {
    let enum_def = elab_enum_def(db, ir.parent(db));
    let name = ir.name(db);
    enum_def
        .variants
        .iter()
        .find(|variant| variant.name == name)
        .unwrap_or_else(|| {
            unreachable!(
                "Cannot find enum variant `{}` in enum `{}`",
                name.contents(db),
                enum_def.name.contents(db)
            )
        })
        .clone()
}

#[salsa::tracked]
/// Synthesise the type of an `Expr::EnumVariant(ir)`
pub fn synth_enum_variant_expr(db: &dyn crate::Db, ir: ir::EnumVariant) -> Arc<Value> {
    let EnumDefSig {
        args: parent_args, ..
    } = enum_def_sig(db, ir.parent(db));

    let EnumVariant {
        args: variant_args,
        ret_type,
        ..
    } = elab_enum_variant(db, ir);

    // convoluted, but saves some unecessary allocations if one or both of the arg
    // lists are empty
    match (parent_args.len(), variant_args.len()) {
        (0, 0) => ret_type.1,
        (_, 0) => Arc::new(Value::FunType(FunClosure::new(
            SharedEnv::new(),
            parent_args,
            Arc::new(ret_type.0),
        ))),
        (0, _) => Arc::new(Value::FunType(FunClosure::new(
            SharedEnv::new(),
            variant_args,
            Arc::new(ret_type.0),
        ))),
        (..) => Arc::new(Value::FunType(FunClosure::new(
            SharedEnv::new(),
            parent_args
                .iter()
                .chain(variant_args.iter())
                .cloned()
                .collect(),
            Arc::new(ret_type.0),
        ))),
    }
}
