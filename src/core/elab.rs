use std::sync::Arc;

use contracts::debug_ensures;
use text_size::TextRange;

use super::env::{EnvLen, LocalEnv, MetaEnv, MetaSource, NameSource, SharedEnv};
use super::eval::{ElimCtx, EvalCtx, EvalOpts};
use super::quote::QuoteCtx;
use super::syntax::*;
use super::unelab::UnelabCtx;
use super::unify::{PartialRenaming, RenameError, SpineError, UnifyCtx, UnifyError};
use crate::ir::diagnostic::IntoFileSpan;
use crate::ir::input_file::InputFile;
use crate::ir::span::Span;
use crate::ir::symbol::Symbol;
use crate::ir::syntax as ir;
use crate::surface::pretty::PrettyCtx;
use crate::surface::syntax as surface;

#[must_use = "Call `.finish()` to report unsolved metas"]
pub struct ElabCtx<'db> {
    local_env: LocalEnv,
    meta_env: MetaEnv,
    renaming: PartialRenaming,
    name_source: NameSource,

    db: &'db dyn crate::Db,
    file: InputFile,
}

impl<'db> ElabCtx<'db> {
    pub fn new(db: &'db dyn crate::Db, file: InputFile) -> Self {
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
                "Help: this function expects {expected_arity} {expected_args} but you gave it  \
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

    fn push_meta_expr(&mut self, name: VarName, source: MetaSource, ty: Arc<Value>) -> Expr {
        let var = self.meta_env.push(name, source, ty);
        Expr::MetaInsertion(var, self.local_env.sources.clone())
    }

    fn push_meta_value(&mut self, name: VarName, source: MetaSource, ty: Arc<Value>) -> Arc<Value> {
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
    fn elab_module(&mut self, module: ir::Module) -> Module {
        let items = module.items(self.db);
        let items = items
            .iter()
            .filter_map(|item| match item {
                ir::Item::Let(let_def) => {
                    let let_def = elab_let_def(self.db, *let_def);
                    Some(Item::Let(let_def))
                }
                ir::Item::Enum(enum_def) => {
                    let enum_def = elab_enum_def(self.db, *enum_def);
                    Some(Item::Enum(enum_def))
                }
                ir::Item::Variant(_) => None,
            })
            .collect();
        Module { items }
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    fn elab_let_def(&mut self, let_def: ir::LetDef) -> LetDef {
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
    fn synth_enum_def(&mut self, enum_def: ir::EnumDef) -> EnumDefSig {
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
                let SynthExpr(ret_core, _) = self.synth_expr(ret_type);
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

    fn elab_enum_def(&mut self, enum_def: ir::EnumDef) -> EnumDef {
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

    fn elab_enum_variant(
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

pub struct SynthExpr(Expr, Arc<Value>);

pub struct CheckExpr(Expr);

/// Expressions
impl ElabCtx<'_> {
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    fn synth_lit(&mut self, lit: &surface::Lit<Span>) -> (Lit, Arc<Value>) {
        match lit {
            surface::Lit::Bool(_, b) => (Lit::Bool(*b), Arc::new(Value::BoolType)),
        }
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    fn synth_error_expr(&mut self) -> SynthExpr {
        // let name = self.name_source.fresh();
        // let source = MetaSource::Error;
        // let ty = self.push_meta_value(name, source, Arc::new(Value::Type));
        SynthExpr(Expr::Error, Arc::new(Value::Error))
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    #[debug_ensures(ret.0.is_closed(self.local_env.len(), self.meta_env.len()))]
    fn synth_expr(&mut self, expr: &surface::Expr<Span>) -> SynthExpr {
        let file = self.file;
        match expr {
            surface::Expr::Error(_) => self.synth_error_expr(),
            surface::Expr::Lit(_, lit) => {
                let (lit, ty) = self.synth_lit(lit);
                SynthExpr(Expr::Lit(lit), ty)
            }
            surface::Expr::Name(span, name) => {
                let symbol = Symbol::intern(self.db, name);

                if let Some((index, ty)) = self.local_env.lookup(symbol) {
                    return SynthExpr(Expr::Local(index), ty);
                }

                if let Some(item) = crate::ir::lookup_item(self.db, file, symbol) {
                    match item {
                        ir::Item::Let(def) => {
                            let def_core = elab_let_def(self.db, def);
                            return SynthExpr(Expr::LetDef(def), def_core.ty.1);
                        }
                        ir::Item::Enum(def) => {
                            let sig = synth_enum_def(self.db, def);
                            let args = sig.args;
                            let ret_type = sig.ret_type;
                            let fun_type = Value::FunType(FunClosure::new(
                                SharedEnv::new(),
                                args,
                                Arc::new(ret_type),
                            ));
                            return SynthExpr(Expr::EnumDef(def), Arc::new(fun_type));
                        }
                        ir::Item::Variant(enum_variant) => {
                            let parent = enum_variant.parent(self.db);
                            let parent_sig = elab_enum_def(self.db, parent).sig;

                            let variant = elab_enum_variant(self.db, enum_variant);
                            let parent_args = parent_sig.args.iter().cloned();
                            let variant_args =
                                variant.args.iter().map(|FunArg { pat, ty }| FunArg {
                                    pat: pat.clone(),
                                    ty: ty.0.clone(),
                                });
                            let args = parent_args.chain(variant_args).collect();
                            let ret_type = variant.ret_type;
                            let fun_type = Value::FunType(FunClosure::new(
                                SharedEnv::new(),
                                args,
                                Arc::new(ret_type.0),
                            ));
                            return SynthExpr(Expr::EnumVariant(enum_variant), Arc::new(fun_type));
                        }
                    }
                }

                match name.as_str() {
                    "Type" => return SynthExpr(Expr::Type, Arc::new(Value::Type)),
                    "Bool" => return SynthExpr(Expr::BoolType, Arc::new(Value::Type)),
                    _ => {}
                }

                crate::error!(span.into_file_span(file), "Unbound variable: `{name}`")
                    .emit(self.db);
                self.synth_error_expr()
            }
            surface::Expr::Hole(span, hole) => {
                let expr_name = match hole {
                    surface::Hole::Underscore => self.name_source.fresh(),
                    surface::Hole::Name(name) => VarName::User(Symbol::new(self.db, name.clone())),
                };
                let type_name = self.name_source.fresh();
                let type_source = MetaSource::HoleType(*span);
                let expr_source = MetaSource::HoleExpr(*span);
                let ty = self.push_meta_value(expr_name, type_source, Arc::new(Value::Type));
                let expr = self.push_meta_expr(type_name, expr_source, ty.clone());
                SynthExpr(expr, ty)
            }
            surface::Expr::FunType(_, pats, ret) => {
                let initial_len = self.local_env.len();
                let fun_args: Vec<_> = pats
                    .iter()
                    .map(|pat| {
                        let SynthPat(pat_core, pat_type) = self.synth_ann_pat(pat);
                        let type_core = self.quote_ctx().quote_value(&pat_type);
                        self.subst_pat(&pat_core, pat_type, None);
                        FunArg {
                            pat: pat_core,
                            ty: type_core,
                        }
                    })
                    .collect();
                let CheckExpr(ret) = self.check_expr_is_type(ret);
                self.local_env.truncate(initial_len);
                SynthExpr(
                    Expr::FunType(Arc::from(fun_args), Arc::new(ret)),
                    Arc::new(Value::Type),
                )
            }
            surface::Expr::FunExpr(_, pats, body) => {
                let initial_len = self.local_env.len();
                let fun_args: Vec<_> = pats
                    .iter()
                    .map(|pat| {
                        let SynthPat(pat_core, pat_type) = self.synth_ann_pat(pat);
                        let type_core = self.quote_ctx().quote_value(&pat_type);
                        self.subst_pat(&pat_core, pat_type, None);
                        FunArg {
                            pat: pat_core,
                            ty: type_core,
                        }
                    })
                    .collect();
                let fun_args: Arc<[_]> = Arc::from(fun_args);

                let SynthExpr(body_core, body_type) = self.synth_expr(body);
                let ret_type = self.quote_ctx().quote_value(&body_type);
                self.local_env.truncate(initial_len);

                let fun_core = Expr::FunExpr(fun_args.clone(), Arc::new(body_core));
                let closure =
                    FunClosure::new(self.local_env.values.clone(), fun_args, Arc::new(ret_type));
                let fun_type = Value::FunType(closure);
                SynthExpr(fun_core, Arc::new(fun_type))
            }
            surface::Expr::FunCall(call_span, fun, args) => {
                let SynthExpr(fun_core, fun_type) = self.synth_expr(fun);
                let fun_type = self.elim_ctx().force_value(&fun_type);
                let closure = match fun_type.as_ref() {
                    Value::FunType(closure) => closure,
                    Value::Error => return self.synth_error_expr(),
                    _ => {
                        self.report_non_fun_call(*call_span, fun.span(), &fun_type);
                        for arg in args {
                            let SynthExpr(..) = self.synth_expr(arg);
                        }
                        return self.synth_error_expr();
                    }
                };

                let expected_arity = closure.arity();
                let actual_arity = args.len();
                if actual_arity != expected_arity {
                    self.report_arity_mismatch(
                        *call_span,
                        fun.span(),
                        actual_arity,
                        expected_arity,
                        &fun_type,
                    );
                    return self.synth_error_expr();
                }

                let initial_closure = closure.clone();
                let mut closure = closure.clone();

                let mut arg_cores = Vec::with_capacity(args.len());
                let mut arg_values = Vec::with_capacity(args.len());
                let mut args = args.iter();

                while let Some((arg, (FunArg { ty, .. }, cont))) =
                    Option::zip(args.next(), self.elim_ctx().split_fun_closure(closure))
                {
                    let CheckExpr(arg_core) = self.check_expr(arg, &ty);
                    let arg_value = self.eval_ctx().eval_expr(&arg_core);
                    closure = cont(arg_value.clone());
                    arg_cores.push(arg_core);
                    arg_values.push(arg_value);
                }

                // Synth the rest of the arguments, in case too many arguments were passed to
                // the function. The result is ignored, but we still need to check them for any
                // errors
                for arg in args {
                    let SynthExpr(..) = self.synth_expr(arg);
                }

                let ret_type = self.elim_ctx().apply_closure(&initial_closure, arg_values);
                SynthExpr(
                    Expr::FunCall(Arc::new(fun_core), Arc::from(arg_cores)),
                    ret_type,
                )
            }
            surface::Expr::Let(_, pat, init, body) => {
                let initial_len = self.local_env.len();
                let SynthPat(pat_core, type_value) = self.synth_ann_pat(pat);
                let type_core = self.quote_ctx().quote_value(&type_value);

                let CheckExpr(init_core) = self.check_expr(init, &type_value);
                let init_value = self.eval_ctx().eval_expr(&init_core);

                self.subst_pat(&pat_core, type_value, Some(init_value));
                let SynthExpr(body_core, body_type) = self.synth_expr(body);
                self.local_env.truncate(initial_len);

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
            surface::Expr::Match(span, scrut, arms) => {
                let name = self.name_source.fresh();
                let source = MetaSource::MatchType(*span);
                let match_type = self.push_meta_value(name, source, Arc::new(Value::Type));

                let CheckExpr(match_expr) = self.check_match_expr(scrut, arms, &match_type);
                SynthExpr(match_expr, match_type)
            }
        }
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    fn check_expr_is_type(&mut self, expr: &surface::Expr<Span>) -> CheckExpr {
        self.check_expr(expr, &Arc::new(Value::Type))
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    #[debug_ensures(ret.0.is_closed(self.local_env.len(), self.meta_env.len()))]
    fn check_expr(&mut self, expr: &surface::Expr<Span>, expected: &Arc<Value>) -> CheckExpr {
        match (expr, expected.as_ref()) {
            (surface::Expr::Error(_), _) => CheckExpr(Expr::Error),
            (surface::Expr::FunExpr(_, pats, body), Value::FunType(closure))
                if pats.len() == closure.arity() =>
            {
                let initial_len = self.local_env.len();
                let initial_closure = closure.clone();
                let mut closure = closure.clone();

                let mut args_values = Vec::with_capacity(pats.len());
                let mut fun_args = Vec::with_capacity(pats.len());

                let mut pats = pats.iter();
                while let Some((pat, (FunArg { ty: expected, .. }, cont))) =
                    Option::zip(pats.next(), self.elim_ctx().split_fun_closure(closure))
                {
                    let type_core = self.quote_ctx().quote_value(&expected);

                    let arg_value = Arc::new(Value::local(self.local_env.len().to_level()));
                    let CheckPat(pat_core) = self.check_ann_pat(pat, &expected);
                    self.subst_pat(&pat_core, expected, None);

                    closure = cont(arg_value.clone());
                    args_values.push(arg_value);
                    fun_args.push(FunArg {
                        pat: pat_core,
                        ty: type_core,
                    });
                }

                let expected_ret = self.elim_ctx().apply_closure(&initial_closure, args_values);
                let CheckExpr(ret_core) = self.check_expr(body, &expected_ret);
                self.local_env.truncate(initial_len);

                CheckExpr(Expr::FunExpr(Arc::from(fun_args), Arc::new(ret_core)))
            }
            (surface::Expr::Let(_, pat, init, body), _) => {
                let initial_len = self.local_env.len();
                let SynthPat(pat_core, type_value) = self.synth_ann_pat(pat);
                let type_core = self.quote_ctx().quote_value(&type_value);

                let CheckExpr(init_core) = self.check_expr(init, &type_value);
                let init_value = self.eval_ctx().eval_expr(&init_core);

                self.subst_pat(&pat_core, type_value, Some(init_value));
                let CheckExpr(body_core) = self.check_expr(body, expected);
                self.local_env.truncate(initial_len);

                CheckExpr(Expr::Let(
                    Arc::new(pat_core),
                    Arc::new(type_core),
                    Arc::new(init_core),
                    Arc::new(body_core),
                ))
            }
            (surface::Expr::Match(_, scrut, arms), _) => {
                self.check_match_expr(scrut, arms, expected)
            }
            _ => {
                let SynthExpr(core, got) = self.synth_expr(expr);
                match self.unify_ctx().unify_values(&got, expected) {
                    Ok(()) => CheckExpr(core),
                    Err(error) => {
                        let span = expr.span();
                        self.report_type_mismatch(span, expected, &got, error);
                        CheckExpr(Expr::Error)
                    }
                }
            }
        }
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    fn check_match_expr(
        &mut self,
        scrut: &surface::Expr<Span>,
        arms: &[(surface::Pat<Span>, surface::Expr<Span>)],
        expected: &Arc<Value>,
    ) -> CheckExpr {
        // TODO: check for exhaustivity and report unreachable patterns

        // FIXME: update `expected` with defintions introduced by `check_pat`
        // without having to quote `expected` back to `Expr`

        let SynthExpr(scrut_core, scrut_type) = self.synth_expr(scrut);
        let scrut_value = self.eval_ctx().eval_expr(&scrut_core);

        let expected_core = self.quote_ctx().quote_value(expected);

        let arms = arms
            .iter()
            .map(|(pat, expr)| {
                let initial_len = self.local_env.len();

                let CheckPat(pat_core) = self.check_pat(pat, &scrut_type);

                self.subst_pat(&pat_core, scrut_type.clone(), Some(scrut_value.clone()));
                let expected = &self.eval_ctx().eval_expr(&expected_core);
                let CheckExpr(expr_core) = self.check_expr(expr, expected);
                self.local_env.truncate(initial_len);

                (pat_core, expr_core)
            })
            .collect();

        CheckExpr(Expr::Match(Arc::new(scrut_core), arms))
    }
}

pub struct SynthPat(Pat, Arc<Value>);

pub struct CheckPat(Pat);

/// Patterns
impl ElabCtx<'_> {
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    fn synth_error_pat(&mut self) -> SynthPat {
        // let name = self.name_source.fresh();
        // let source = MetaSource::Error;
        // let ty = self.push_meta_value(name, source, Arc::new(Value::Type));
        SynthPat(Pat::Error, Arc::new(Value::Error))
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    fn synth_pat(&mut self, pat: &surface::Pat<Span>) -> SynthPat {
        let file = self.file;
        let db = self.db;
        let mut meta = || {
            let name = self.name_source.fresh();
            let span = pat.span();
            let source = MetaSource::PatType(span);
            self.push_meta_value(name, source, Arc::new(Value::Type))
        };

        match pat {
            surface::Pat::Error(_) => SynthPat(Pat::Name(VarName::Underscore), meta()),
            surface::Pat::Wildcard(_) => SynthPat(Pat::Name(VarName::Underscore), meta()),
            surface::Pat::Name(_, name) => {
                let name = VarName::User(Symbol::intern(db, name));
                SynthPat(Pat::Name(name), meta())
            }
            surface::Pat::Lit(_, lit) => {
                let (lit, ty) = self.synth_lit(lit);
                SynthPat(Pat::Lit(lit), ty)
            }
            surface::Pat::Variant(span, name, pats) => {
                let symbol = Symbol::intern(self.db, name);
                match crate::ir::lookup_item(self.db, self.file, symbol) {
                    Some(ir::Item::Variant(variant)) => {
                        let EnumVariant { args, ret_type, .. } =
                            elab_enum_variant(self.db, variant);
                        let pats = pats
                            .iter()
                            .zip(args.iter().map(|FunArg { pat, ty }| FunArg {
                                pat: pat.clone(),
                                ty: ty.1.clone(),
                            }))
                            .map(|(pat, arg)| {
                                let CheckPat(pat) = self.check_pat(pat, &arg.ty);
                                pat
                            })
                            .collect();
                        SynthPat(Pat::Variant(variant, pats), ret_type.1)
                    }
                    _ => {
                        crate::error!(span.into_file_span(file), "Unbound variable: `{name}`")
                            .emit(db);
                        self.synth_error_pat()
                    }
                }
            }
        }
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    fn check_pat(&mut self, pat: &surface::Pat<Span>, expected: &Arc<Value>) -> CheckPat {
        let expected = self.elim_ctx().force_value(expected);
        match pat {
            surface::Pat::Error(_) => CheckPat(Pat::Name(VarName::Underscore)),
            surface::Pat::Wildcard(_) => CheckPat(Pat::Name(VarName::Underscore)),
            surface::Pat::Name(_, name) => {
                let name = VarName::User(Symbol::intern(self.db, name));
                CheckPat(Pat::Name(name))
            }
            _ => {
                let SynthPat(core, got) = self.synth_pat(pat);
                match self.unify_ctx().unify_values(&got, &expected) {
                    Ok(_) => CheckPat(core),
                    Err(error) => {
                        let span = pat.span();
                        self.report_type_mismatch(span, &expected, &got, error);
                        CheckPat(Pat::Error)
                    }
                }
            }
        }
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    fn synth_ann_pat(&mut self, pat: &surface::AnnPat<Span>) -> SynthPat {
        let surface::AnnPat { pat, ty } = pat;
        match ty {
            None => self.synth_pat(pat),
            Some(ty) => {
                let CheckExpr(type_core) = self.check_expr_is_type(ty);
                let type_value = self.eval_ctx().eval_expr(&type_core);
                let CheckPat(pat_core) = self.check_pat(pat, &type_value);
                SynthPat(pat_core, type_value)
            }
        }
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    fn check_ann_pat(&mut self, pat: &surface::AnnPat<Span>, expected: &Arc<Value>) -> CheckPat {
        let surface::AnnPat { pat, ty } = pat;
        match ty {
            None => self.check_pat(pat, expected),
            Some(ty) => {
                let CheckExpr(type_core) = self.check_expr_is_type(ty);
                let type_value = self.eval_ctx().eval_expr(&type_core);

                if let Err(error) = self.unify_ctx().unify_values(&type_value, expected) {
                    let span = pat.span();
                    self.report_type_mismatch(span, expected, &type_value, error);
                }

                self.check_pat(pat, &type_value)
            }
        }
    }

    fn subst_pat(&mut self, pat: &Pat, ty: Arc<Value>, value: Option<Arc<Value>>) {
        match (pat, ty.as_ref(), value.as_ref()) {
            (Pat::Error, ..) => {
                self.local_env.push(VarName::Underscore, ty, value);
            }
            (Pat::Name(name), ..) => {
                self.local_env.push(*name, ty, value);
            }
            (Pat::Lit(lit), ..) => {
                let pat_value = Arc::new(Value::Lit(lit.clone()));
                let name = self.name_source.fresh();
                self.local_env.push(name, ty, Some(pat_value));
            }
            (Pat::Variant(..), ..) => todo!(),
            _ => unreachable!("Cannot subst {value:#?} with type {ty:#?} into {pat:#?}"),
        }
    }
}
