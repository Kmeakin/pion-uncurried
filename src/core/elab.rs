use std::sync::Arc;

use contracts::debug_ensures;

use super::env::{EnvLen, LocalEnv, MetaEnv, MetaSource, NameSource, SharedEnv};
use super::eval::{ElimCtx, EvalCtx, EvalOpts};
use super::quote::QuoteCtx;
use super::syntax::*;
use super::unelab::UnelabCtx;
use super::unify::{PartialRenaming, RenameError, SpineError, UnifyCtx, UnifyError};
use crate::file::File;
use crate::span::{IntoFileSpan, Span};
use crate::surface::pretty::PrettyCtx;
use crate::symbol::Symbol;
use crate::{ir, surface};

mod expr;
mod item;
mod pat;

use self::expr::*;
pub use self::item::*;
use self::pat::*;

#[must_use = "Call `.finish()` to report unsolved metas"]
pub struct ElabCtx<'db> {
    local_env: LocalEnv,
    meta_env: MetaEnv,
    renaming: PartialRenaming,
    name_source: NameSource,

    db: &'db dyn crate::Db,
    file: File,
}

impl<'db> ElabCtx<'db> {
    pub fn new(db: &'db dyn crate::Db, file: File) -> Self {
        Self {
            local_env: LocalEnv::new(),
            meta_env: MetaEnv::new(),
            renaming: PartialRenaming::default(),
            name_source: NameSource::default(),

            db,
            file,
        }
    }

    pub fn report_unsolved_metas(&self) {
        for it in self
            .meta_env
            .values
            .iter()
            .zip(self.meta_env.sources.iter())
        {
            let (span, name) = match it {
                (Some(_), _) => continue,
                (None, MetaSource::Error) => continue,
                (None, MetaSource::HoleType(span)) => (*span, "type of hole"),
                (None, MetaSource::HoleExpr(span)) => (*span, "expr of hole"),
                (None, MetaSource::PatType(span)) => (*span, "type of pattern"),
                (None, MetaSource::MatchType(span)) => (*span, "type of `match` expression"),
            };
            crate::error!(span.into_file_span(self.file), "Unable to infer {name}").emit(self.db);
        }
    }

    fn report_type_mismatch(
        &mut self,
        span: Span,
        expected: &Arc<Value>,
        actual: &Arc<Value>,
        error: UnifyError,
    ) {
        let expected = self.pretty_value(expected);
        let actual = self.pretty_value(actual);
        let filespan = span.into_file_span(self.file);
        let builder = crate::error!(filespan, "Type mismatch")
            .skip_primary_label()
            .with_secondary_label(span, format!("Help: expected {expected}, got {actual}"));
        let builder =
            match error {
                UnifyError::Mismatch => builder,
                UnifyError::Spine(error) => match error {
                    SpineError::NonLinearSpine(_) => builder.with_secondary_label(
                        span,
                        "variable appeared more than once in problem spine",
                    ),
                    SpineError::NonRigidSpine => {
                        builder.with_secondary_label(span, "meta variable found in problem spine")
                    }
                    SpineError::Match => builder
                        .with_secondary_label(span, "`match` expression found in problem spine"),
                },
                UnifyError::Rename(error) => match error {
                    RenameError::EscapingLocalVar(_) => {
                        builder.with_secondary_label(span, "local variable escapes solution")
                    }
                    RenameError::InfiniteSolution => builder
                        .with_secondary_label(span, "attempted to construct infinite solution"),
                },
            };
        builder.emit(self.db);
    }

    fn report_non_fun_call(&mut self, call_span: Span, fun_span: Span, fun_type: &Arc<Value>) {
        let fun_span = fun_span.into_file_span(self.file);
        let call_span = call_span.into_file_span(self.file);
        let fun_type = self.pretty_value(fun_type);
        crate::error!(call_span, "Called non-function expression")
            .skip_primary_label()
            .with_secondary_label(
                fun_span,
                format!("Help: type of this expression is `{fun_type}`"),
            )
            .emit(self.db);
    }

    fn report_arity_mismatch(
        &mut self,
        call_span: Span,
        fun_span: Span,
        actual_arity: usize,
        expected_arity: usize,
        fun_type: &Arc<Value>,
    ) {
        let fun_span = fun_span.into_file_span(self.file);
        let call_span = call_span.into_file_span(self.file);
        let fun_type = self.pretty_value(fun_type);
        let few_or_many = if actual_arity < expected_arity {
            "few"
        } else {
            "many"
        };
        let expected_args = if expected_arity == 1 {
            "argument"
        } else {
            "arguments"
        };
        crate::error!(
            call_span,
            "Called function with too {few_or_many} arguments"
        )
        .skip_primary_label()
        .with_secondary_label(
            fun_span,
            format!(
                "Help: this function expects {expected_arity} {expected_args} but you gave it \
                 {actual_arity}"
            ),
        )
        .with_secondary_label(
            fun_span,
            format!("Help: type of this function is `{fun_type}`"),
        )
        .emit(self.db);
    }
}

#[salsa::tracked]
#[cfg(FALSE)]
pub fn elab_module(db: &dyn crate::Db, module: ir::Module) -> Module {
    let file = module.file(db);
    let mut ctx = ElabCtx::new(db, file);
    let module = ctx.elab_module(module);
    ctx.report_unsolved_metas();
    module
}

#[salsa::tracked]
#[cfg(FALSE)]
pub fn synth_enum_def(db: &dyn crate::Db, enum_def: ir::EnumDef) -> EnumDefSig {
    let file = enum_def.file(db);
    let mut ctx = ElabCtx::new(db, file);
    let sig = ctx.synth_enum_def(enum_def);
    ctx.report_unsolved_metas();
    sig
}

#[salsa::tracked]
#[cfg(FALSE)]
pub fn elab_enum_def(db: &dyn crate::Db, enum_def: ir::EnumDef) -> EnumDef {
    let file = enum_def.file(db);
    let mut ctx = ElabCtx::new(db, file);
    let enum_def = ctx.elab_enum_def(enum_def);
    ctx.report_unsolved_metas();
    enum_def
}

#[salsa::tracked]
#[cfg(FALSE)]
pub fn elab_enum_variant(db: &dyn crate::Db, variant: ir::EnumVariant) -> EnumVariant {
    let file = variant.file(db);
    let mut ctx = ElabCtx::new(db, file);
    let sig = ctx.synth_enum_def(variant.parent(db));
    let variant = ctx.elab_enum_variant(variant.surface(db), &sig);
    ctx.report_unsolved_metas();
    variant
}

/// Helpers
impl ElabCtx<'_> {
    pub fn eval_ctx(&mut self) -> EvalCtx {
        EvalCtx::new(
            &mut self.local_env.values,
            &self.meta_env.values,
            EvalOpts::EVAL_CBV,
            self.db,
        )
    }

    pub fn elim_ctx(&self) -> ElimCtx {
        ElimCtx::new(&self.meta_env.values, EvalOpts::EVAL_CBV, self.db)
    }

    pub fn quote_ctx(&mut self) -> QuoteCtx {
        QuoteCtx::new(self.local_env.values.len(), &self.meta_env.values, self.db)
    }

    pub fn unify_ctx(&mut self) -> UnifyCtx {
        UnifyCtx::new(
            self.local_env.values.len(),
            &mut self.meta_env.values,
            &mut self.renaming,
            self.db,
        )
    }

    pub fn unelab_ctx(&mut self) -> UnelabCtx<'_> {
        UnelabCtx::new(
            &mut self.local_env.names,
            &self.meta_env.names,
            &mut self.name_source,
            self.db,
        )
    }

    pub fn push_meta_expr(&mut self, name: VarName, source: MetaSource, ty: Arc<Value>) -> Expr {
        let var = self.meta_env.push(name, source, ty);
        Expr::MetaInsertion(var, self.local_env.sources.clone())
    }

    pub fn push_meta_value(
        &mut self,
        name: VarName,
        source: MetaSource,
        ty: Arc<Value>,
    ) -> Arc<Value> {
        let expr = self.push_meta_expr(name, source, ty);
        self.eval_ctx().eval_expr(&expr)
    }

    pub fn pretty_value(&mut self, value: &Arc<Value>) -> String {
        let core = self.quote_ctx().quote_value(value);
        let surface = self.unelab_ctx().unelab_expr(&core);
        let pretty_ctx = PrettyCtx::new();
        let doc = pretty_ctx.pretty_expr(&surface).into_doc();
        doc.pretty(80).to_string()
    }
}

/// Items
impl ElabCtx<'_> {
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    #[cfg(FALSE)]
    pub fn elab_let_def(&mut self, let_def: ir::LetDef) -> LetDef {
        let name = let_def.name(self.db);
        let surface::LetDef { ty, body, .. } = let_def.surface(self.db);

        let (body_expr, type_value) = match ty {
            Some(ty) => {
                let CheckExpr(type_expr) = self.check_expr_is_type(ty);
                let type_value = self.eval_ctx().eval_expr(&type_expr);
                let CheckExpr(body_expr) = self.check_expr(body, &type_value);
                (body_expr, type_value)
            }
            None => {
                let SynthExpr(body_expr, type_value) = self.synth_expr(body);
                (body_expr, type_value)
            }
        };
        let type_expr = self.quote_ctx().quote_value(&type_value);

        let expr_opts = EvalOpts {
            beta_reduce: true, // TODO: disable beta reduction
            ..EvalOpts::ZONK
        };
        let value_opts = EvalOpts {
            error_on_unsolved_meta: true,
            ..EvalOpts::EVAL_CBV
        };

        let body_value = self.eval_ctx().with_opts(value_opts).eval_expr(&body_expr);
        let (body_expr, _) = self
            .eval_ctx()
            .with_opts(expr_opts)
            .normalize_expr(&body_expr);

        let type_value = self.eval_ctx().with_opts(value_opts).eval_expr(&type_expr);
        let (type_expr, _) = self
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

    // NOTE: does not truncate env
    #[cfg(FALSE)]
    pub fn synth_enum_def(&mut self, enum_def: ir::EnumDef) -> EnumDefSig {
        let surface::EnumDef { args, ret_type, .. } = enum_def.surface(self.db);

        let mut self_args = Vec::with_capacity(args.len());
        let args: Arc<[_]> = args
            .iter()
            .map(|pat| {
                let SynthPat(pat_core, type_value) = self.synth_ann_pat(pat);
                let type_core = self.quote_ctx().quote_value(&type_value);
                self_args.push(Arc::new(Value::local(self.local_env.len().to_level())));
                self.subst_pat(&pat_core, type_value, None);
                FunArg {
                    pat: pat_core,
                    ty: (type_core),
                }
            })
            .collect();

        let self_type = Value::Stuck(
            Head::EnumDef(enum_def),
            if args.len() == 0 {
                vec![]
            } else {
                vec![Elim::FunCall(self_args)]
            },
        );
        let self_type = Arc::new(self_type);

        let ret_type = match ret_type {
            Some(ret_type) => {
                let SynthExpr(ret_core, _) = self.synth_expr(&ret_type);
                let ret_value = self.eval_ctx().eval_expr(&ret_core);
                let expected = Arc::new(Value::Type);
                match self.unify_ctx().unify_values(&ret_value, &expected) {
                    Ok(_) => {
                        let type_core = self.quote_ctx().quote_value(&ret_value);
                        type_core
                    }
                    Err(error) => {
                        self.report_type_mismatch(ret_type.span(), &expected, &ret_value, error);
                        Expr::Error
                    }
                }
            }
            None => Expr::Type,
        };

        EnumDefSig {
            args,
            ret_type,
            self_type,
        }
    }

    #[cfg(FALSE)]
    pub fn elab_enum_def(&mut self, enum_def: ir::EnumDef) -> EnumDef {
        let initial_len = self.local_env.len();
        let name = enum_def.name(self.db);
        let surface::EnumDef { variants, .. } = enum_def.surface(self.db);

        let sig = &self.synth_enum_def(enum_def);

        let variants = variants
            .iter()
            .map(|variant| self.elab_enum_variant(variant, sig))
            .collect();
        self.local_env.truncate(initial_len);

        EnumDef {
            name,
            sig: sig.clone(),
            variants,
        }
    }

    #[cfg(FALSE)]
    pub fn elab_enum_variant(
        &mut self,
        enum_variant: &surface::EnumVariant<TextRange>,
        sig: &EnumDefSig,
    ) -> EnumVariant {
        let surface::EnumVariant {
            name,
            args,
            ret_type: ty,
        } = enum_variant;

        let initial_len = self.local_env.len();
        let name = Symbol::new(self.db, name.to_owned());
        let args: Arc<[_]> = args
            .iter()
            .map(|pat| {
                let SynthPat(pat_core, type_value) = self.synth_ann_pat(pat);
                let type_core = self.quote_ctx().quote_value(&type_value);
                self.subst_pat(&pat_core, type_value.clone(), None);
                FunArg {
                    pat: pat_core,
                    ty: (type_core, type_value),
                }
            })
            .collect();

        let ret_type = match ty {
            Some(ty) => {
                let CheckExpr(ret_core) = self.check_expr_is_type(ty);
                let ret_value = self.eval_ctx().eval_expr(&ret_core);
                (self.quote_ctx().quote_value(&ret_value), ret_value)
            }
            None => (
                self.quote_ctx().quote_value(&sig.self_type),
                sig.self_type.clone(),
            ),
        };
        self.local_env.truncate(initial_len);

        EnumVariant {
            name,
            args,
            ret_type,
        }
    }
}
