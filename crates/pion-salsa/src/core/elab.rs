use std::sync::Arc;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use contracts::debug_ensures;

use super::env::{LocalEnv, MetaEnv, MetaSource, NameSource};
use super::eval::{ElimCtx, EvalCtx};
use super::quote::QuoteCtx;
use super::syntax::*;
use super::unify::{PartialRenaming, RenameError, SpineError, UnifyCtx, UnifyError};
use crate::ir::input_file::InputFile;
use crate::ir::span::Span;
use crate::ir::symbol::Symbol;
use crate::ir::syntax as ir;
use crate::surface::syntax as surface;

pub struct ElabCtx<'db> {
    local_env: LocalEnv,
    meta_env: MetaEnv,
    renaming: PartialRenaming,
    name_source: NameSource,
    diagnostics: Vec<Diagnostic<InputFile>>,

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
            diagnostics: Vec::new(),

            db,
            file,
        }
    }

    pub fn finish(mut self) -> Vec<Diagnostic<InputFile>> {
        let unsolved_metas = self
            .meta_env
            .values
            .iter()
            .zip(self.meta_env.sources.iter())
            .filter_map(move |it| {
                let (span, name) = match it {
                    (Some(_), _) => return None,
                    (None, MetaSource::Error) => return None,
                    (None, MetaSource::PatType(span)) => (*span, "type of pattern"),
                    (None, MetaSource::MatchType(span)) => (*span, "type of `match` expression"),
                };
                Some(
                    Diagnostic::error()
                        .with_message(format!("Unable to infer {name}"))
                        .with_labels(vec![Label::primary(self.file, span)]),
                )
            });
        self.diagnostics.extend(unsolved_metas);
        self.diagnostics
    }

    fn report_type_mismatch(
        &mut self,
        span: Span,
        _expected: &Value,
        _actual: &Value,
        error: UnifyError,
    ) {
        let diag = match error {
            UnifyError::Mismatch => Diagnostic::error()
                .with_message("Type mismatch")
                .with_labels(vec![Label::primary(self.file, span)]),
            UnifyError::Spine(error) => match error {
                SpineError::NonLinearSpine(_) => Diagnostic::error()
                    .with_message(
                        "Unification error: variable appeared more than once in problem spine",
                    )
                    .with_labels(vec![Label::primary(self.file, span)]),
                SpineError::NonRigidSpine => Diagnostic::error()
                    .with_message("Unification error: meta variable found in problem spine")
                    .with_labels(vec![Label::primary(self.file, span)]),
                SpineError::Match => Diagnostic::error()
                    .with_message("Unification error: match-expression found in problem spine")
                    .with_labels(vec![Label::primary(self.file, span)]),
            },
            UnifyError::Rename(error) => match error {
                RenameError::EscapingLocalVar(_) => Diagnostic::error()
                    .with_message("Unification error: local variable escapes solution")
                    .with_labels(vec![Label::primary(self.file, span)]),
                RenameError::InfiniteSolution => Diagnostic::error()
                    .with_message("Unification error: attempted to construct infinite solution")
                    .with_labels(vec![Label::primary(self.file, span)]),
            },
        };
        self.diagnostics.push(diag);
    }
}

#[salsa::tracked]
pub fn elab_let_def(db: &dyn crate::Db, let_def: ir::LetDef) -> LetDef {
    let file = let_def.file(db);
    let surface = let_def.surface(db);
    let type_expr = &surface.ty;
    let body_expr = &surface.expr;

    let mut ctx = ElabCtx::new(db, file);

    match type_expr {
        Some(type_expr) => {
            let CheckExpr(type_core) = ctx.check_expr_is_type(type_expr);
            let type_value = ctx.eval_ctx().eval_expr(&type_core);

            let CheckExpr(body_core) = ctx.check_expr(body_expr, &type_value);
            let body_value = ctx.eval_ctx().eval_expr(&body_core);
            let forced_body_value = ctx.elim_ctx().force_value(&body_value);
            let forced_body_expr = ctx.quote_ctx().quote_value(&forced_body_value);

            let forced_type_value = ctx.elim_ctx().force_value(&type_value);
            let forced_type_expr = ctx.quote_ctx().quote_value(&forced_type_value);

            let diagnostics = ctx.finish();

            LetDef {
                body: (forced_body_expr, forced_body_value),
                ty: (forced_type_expr, forced_type_value),
                diagnostics,
            }
        }
        None => {
            let SynthExpr(body_core, body_type) = ctx.synth_expr(body_expr);
            let body_value = ctx.eval_ctx().eval_expr(&body_core);

            let forced_body_value = ctx.elim_ctx().force_value(&body_value);
            let forced_body_expr = ctx.quote_ctx().quote_value(&forced_body_value);

            let forced_type_value = ctx.elim_ctx().force_value(&body_type);
            let forced_type_expr = ctx.quote_ctx().quote_value(&forced_type_value);

            let diagnostics = ctx.finish();

            LetDef {
                body: (forced_body_expr, forced_body_value),
                ty: (forced_type_expr, forced_type_value),
                diagnostics,
            }
        }
    }
}

impl ElabCtx<'_> {
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

    fn push_meta_expr(&mut self, name: VarName, source: MetaSource, ty: Arc<Value>) -> Expr {
        let var = self.meta_env.push(name, source, ty);
        Expr::MetaInsertion(var, self.local_env.sources.clone())
    }

    fn push_meta_value(&mut self, name: VarName, source: MetaSource, ty: Arc<Value>) -> Arc<Value> {
        let expr = self.push_meta_expr(name, source, ty);
        self.eval_ctx().eval_expr(&expr)
    }
}

pub struct SynthExpr(Expr, Arc<Value>);

pub struct CheckExpr(Expr);

impl ElabCtx<'_> {
    fn synth_lit(&mut self, lit: &surface::Lit) -> (Lit, Arc<Value>) {
        match lit {
            surface::Lit::Bool(b) => (Lit::Bool(*b), Arc::new(Value::BoolType)),
        }
    }

    fn synth_error_expr(&mut self) -> SynthExpr {
        let name = self.name_source.fresh();
        let source = MetaSource::Error;
        let ty = self.push_meta_value(name, source, Arc::new(Value::Type));
        SynthExpr(Expr::Error, ty)
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
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

                match name.as_str() {
                    "Type" => return SynthExpr(Expr::Type, Arc::new(Value::Type)),
                    "Bool" => return SynthExpr(Expr::BoolType, Arc::new(Value::Type)),
                    _ => {}
                }

                self.diagnostics.push(
                    Diagnostic::error()
                        .with_message(format!("Unbound variable: `{name}`"))
                        .with_labels(vec![Label::primary(file, *span)]),
                );

                self.synth_error_expr()
            }
            surface::Expr::FunType(_, pats, ret) => {
                let initial_len = self.local_env.len();
                let args: Vec<_> = pats
                    .iter()
                    .map(|pat| {
                        let SynthPat(pat_core, pat_type) = self.synth_ann_pat(pat);
                        let type_core = self.quote_ctx().quote_value(&pat_type);
                        self.subst_pat(&pat_core, pat_type, None);
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
            surface::Expr::FunExpr(_, pats, body) => {
                let initial_len = self.local_env.len();
                let args: Vec<_> = pats
                    .iter()
                    .map(|pat| {
                        let SynthPat(pat_core, pat_type) = self.synth_ann_pat(pat);
                        let type_core = self.quote_ctx().quote_value(&pat_type);
                        self.subst_pat(&pat_core, pat_type, None);
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
            surface::Expr::FunCall(span, fun, args) => {
                let SynthExpr(fun_core, fun_type) = self.synth_expr(fun);
                let fun_type = self.elim_ctx().force_value(&fun_type);
                let closure = match fun_type.as_ref() {
                    Value::FunType(closure) => closure,
                    Value::Error => return self.synth_error_expr(),
                    _ => {
                        let fun_span = fun.span();
                        self.diagnostics.push(
                            Diagnostic::error()
                                .with_message("Called non-function expression")
                                .with_labels(vec![Label::primary(file, fun_span)]),
                        );
                        return self.synth_error_expr();
                    }
                };

                let expected_arity = closure.arity();
                let actual_arity = args.len();
                if actual_arity != expected_arity {
                    self.diagnostics.push(
                        Diagnostic::error()
                            .with_message(format!(
                                "Function expects {expected_arity} arguments but {actual_arity} \
                                 arguments were supplied"
                            ))
                            .with_labels(vec![Label::primary(file, *span)]),
                    );
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

    fn check_expr_is_type(&mut self, expr: &surface::Expr<Span>) -> CheckExpr {
        self.check_expr(expr, &Arc::new(Value::Type))
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
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
                let mut arg_types = Vec::with_capacity(pats.len());

                let mut pats = pats.iter();
                while let Some((pat, (expected, cont))) =
                    Option::zip(pats.next(), self.elim_ctx().split_fun_closure(closure))
                {
                    let type_core = self.quote_ctx().quote_value(&expected);

                    let arg_value = Arc::new(Value::local(self.local_env.len().to_level()));
                    let CheckPat(pat_core) = self.check_ann_pat(pat, &expected);
                    self.subst_pat(&pat_core, expected, None);

                    closure = cont(arg_value.clone());
                    args_values.push(arg_value);
                    arg_types.push(type_core);
                }

                let expected_ret = self.elim_ctx().apply_closure(&initial_closure, args_values);
                let CheckExpr(ret_core) = self.check_expr(body, &expected_ret);
                self.local_env.truncate(initial_len);

                CheckExpr(Expr::FunExpr(Arc::from(arg_types), Arc::new(ret_core)))
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

impl ElabCtx<'_> {
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    fn synth_pat(&mut self, pat: &surface::Pat<Span>) -> SynthPat {
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
        match pat {
            Pat::Error => {
                self.local_env.push(VarName::Underscore, ty, value);
            }
            Pat::Name(name) => {
                self.local_env.push(*name, ty, value);
            }
            Pat::Lit(_) => {
                let name = self.name_source.fresh();
                self.local_env.push(name, ty, value);
            }
        }
    }
}
