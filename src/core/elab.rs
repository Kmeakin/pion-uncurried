use std::sync::Arc;

use contracts::debug_ensures;
use text_size::TextRange;

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

    pub fn finish(self) {
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
pub fn elab_module(db: &dyn crate::Db, module: ir::Module) -> Module {
    let file = module.file(db);
    let mut ctx = ElabCtx::new(db, file);
    let module = ctx.elab_module(module);
    ctx.finish();
    module
}

#[salsa::tracked]
pub fn elab_let_def(db: &dyn crate::Db, let_def: ir::LetDef) -> LetDef {
    let file = let_def.file(db);
    let mut ctx = ElabCtx::new(db, file);
    let let_def = ctx.elab_let_def(let_def);
    ctx.finish();
    let_def
}

#[salsa::tracked]
pub fn synth_enum_def(db: &dyn crate::Db, enum_def: ir::EnumDef) -> EnumDefSig {
    let file = enum_def.file(db);
    let mut ctx = ElabCtx::new(db, file);
    let sig = ctx.synth_enum_def(enum_def);
    ctx.finish();
    sig
}

#[salsa::tracked]
pub fn elab_enum_def(db: &dyn crate::Db, enum_def: ir::EnumDef) -> EnumDef {
    let file = enum_def.file(db);
    let mut ctx = ElabCtx::new(db, file);
    let enum_def = ctx.elab_enum_def(enum_def);
    ctx.finish();
    enum_def
}

#[salsa::tracked]
pub fn elab_enum_variant(db: &dyn crate::Db, variant: ir::EnumVariant) -> EnumVariant {
    let file = variant.file(db);
    let mut ctx = ElabCtx::new(db, file);
    let sig = ctx.synth_enum_def(variant.parent(db));
    let variant = ctx.elab_enum_variant(variant.surface(db), &sig);
    ctx.finish();
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
