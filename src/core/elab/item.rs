#![allow(clippy::needless_borrow)]

use super::*;
use crate::core::semantics::binders::IsClosed;
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetDefSig {
    r#type: (Expr, Arc<Value>),
    subst: (LocalEnv, MetaEnv, PartialRenaming),
}

#[salsa::tracked]
pub fn let_def_sig(db: &dyn crate::Db, ir: ir::LetDef) -> LetDefSig {
    let mut ctx = ElabCtx::new(db, ir.file(db));
    let surface::LetDef { type_: r#type, .. } = ir.surface(db);

    let (type_expr, type_value) = match r#type {
        Some(r#type) => {
            let CheckExpr(type_expr) = ctx.check_expr_is_type(&r#type);
            let type_value = ctx.eval_ctx().eval_expr(&type_expr);
            (type_expr, type_value)
        }
        None => {
            let name = VarName::Generated("let-type");
            let source = MetaSource::LetDefType(ir);
            let type_expr = ctx.push_meta_expr(name, source, Arc::new(Value::TYPE));
            let type_value = ctx.eval_ctx().eval_expr(&type_expr);
            (type_expr, type_value)
        }
    };

    LetDefSig {
        r#type: (type_expr, type_value),
        subst: (ctx.local_env, ctx.meta_env, ctx.renaming),
    }
}

#[salsa::tracked]
pub fn elab_let_def(db: &dyn crate::Db, ir: ir::LetDef) -> LetDef {
    let surface::LetDef {
        name: (_, name),
        body,
        ..
    } = ir.surface(db);
    let name = Symbol::new(db, name.to_owned());

    let LetDefSig {
        r#type: (type_expr, type_value),
        subst,
    } = let_def_sig(db, ir);
    let mut ctx = ElabCtx::new(db, ir.file(db));
    ctx.local_env = subst.0;
    ctx.meta_env = subst.1;
    ctx.renaming = subst.2;

    let CheckExpr(body_expr) = ctx.check_expr(&body, &type_value);

    let type_expr = ctx
        .eval_ctx()
        .with_flags(EvalFlags::ZONK)
        .zonk_expr(&type_expr);
    let body_expr = ctx
        .eval_ctx()
        .with_flags(EvalFlags::ZONK)
        .zonk_expr(&body_expr);
    let type_value = ctx.eval_ctx().eval_expr(&type_expr);
    let body_value = ctx.eval_ctx().eval_expr(&body_expr);

    debug_assert!(
        body_expr.is_closed(EnvLen(0), EnvLen(0)),
        "free variables in `body_expr`: {body_expr:#?}"
    );
    debug_assert!(
        type_expr.is_closed(EnvLen(0), EnvLen(0)),
        "free variables in `type_expr`: {type_expr:#?}"
    );

    ctx.report_unsolved_metas();
    LetDef {
        name,
        body: (body_expr, body_value),
        r#type: (type_expr, type_value),
    }
}

#[salsa::tracked]
/// Synthesise the type of an `Expr::EnumDef(ir)`
pub fn synth_let_def_expr(db: &dyn crate::Db, ir: ir::LetDef) -> Arc<Value> {
    let LetDef { r#type, .. } = elab_let_def(db, ir);
    r#type.1
}

#[salsa::tracked]
pub fn eval_let_def_expr(db: &dyn crate::Db, ir: ir::LetDef) -> Arc<Value> {
    let LetDef { body, .. } = elab_let_def(db, ir);
    ElabCtx::new(db, ir.file(db)).eval_ctx().eval_expr(&body.0)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDefSig {
    pub args: Telescope<Expr>,
    pub arg_values: Telescope<Arc<Value>>,
    pub ret_type: (Expr, Arc<Value>),
    pub self_type: Arc<Value>,
    pub subst: (LocalEnv, MetaEnv, PartialRenaming),
}

#[salsa::tracked]
/// The non recursive part of `elab_enum_def`. Crucial for "tying the knot" of
/// recursive enums.
pub fn enum_def_sig(db: &dyn crate::Db, ir: ir::EnumDef) -> EnumDefSig {
    let mut ctx = ElabCtx::new(db, ir.file(db));
    let surface::EnumDef { args, ret_type, .. } = ir.surface(db);

    let mut self_type_args = Vec::with_capacity(args.len());
    let mut args_exprs = Vec::with_capacity(args.len());
    let mut arg_values = Vec::with_capacity(args.len());

    for arg in args {
        let SynthPat(pat_core, type_value) = ctx.synth_ann_pat(&arg);
        let type_core = ctx.quote_ctx().quote_value(&type_value);

        self_type_args.push(Arc::new(Value::local(ctx.local_env.len().to_level())));
        ctx.push_pat_params(&pat_core, type_value.clone());

        args_exprs.push(FunArg {
            pat: pat_core.clone(),
            r#type: type_core,
        });
        arg_values.push(FunArg {
            pat: pat_core,
            r#type: type_value,
        });
    }

    let arg_exprs = Telescope(Arc::from(args_exprs));
    let arg_values = Telescope(Arc::from(arg_values));

    let (ret_type_expr, ret_type_value) = match ret_type {
        None => (Expr::Prim(Prim::Type), Arc::new(Value::TYPE)),
        Some(ret_type_surface) => {
            let SynthExpr(ret_type_expr, _) = ctx.synth_expr(&ret_type_surface);
            let ret_type_value = ctx.eval_ctx().eval_expr(&ret_type_expr);
            let expected = Arc::new(Value::TYPE);
            match ctx.unify_ctx().unify_values(&ret_type_value, &expected) {
                Ok(_) => (ret_type_expr, ret_type_value),
                Err(error) => {
                    ctx.report_type_mismatch(
                        ret_type_surface.span(),
                        &expected,
                        &ret_type_value,
                        error,
                    );
                    (Expr::ERROR, Arc::new(Value::ERROR))
                }
            }
        }
    };

    let self_type = match self_type_args.len() {
        0 => Arc::new(Value::enum_def(ir)),
        _ => Arc::new(Value::Stuck(
            Head::Global(GlobalVar::Enum(ir)),
            vec![Elim::FunCall(self_type_args)],
        )),
    };

    ctx.report_unsolved_metas();
    EnumDefSig {
        args: arg_exprs,
        arg_values,
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
        ..
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

            let mut arg_exprs = Vec::new();
            let mut arg_values = Vec::new();
            for arg in args {
                let SynthPat(pat_core, pat_type) = ctx.synth_ann_pat(arg);
                let type_core = ctx.quote_ctx().quote_value(&pat_type);
                ctx.push_pat_params(&pat_core, pat_type.clone());
                arg_exprs.push(FunArg {
                    pat: pat_core.clone(),
                    r#type: type_core,
                });
                arg_values.push(FunArg {
                    pat: pat_core,
                    r#type: pat_type,
                });
            }
            let arg_exprs = Telescope(Arc::from(arg_exprs));
            let arg_values = Telescope(Arc::from(arg_values));

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
                args: arg_exprs,
                arg_values,
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
