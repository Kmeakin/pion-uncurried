use std::rc::Rc;

use contracts::debug_ensures;
use text_size::TextRange;

use super::conv::ConvCtx;
use super::env::{EnumEnv, ItemEnv, LocalEnv, MetaEnv};
use super::errors::ElabError;
use super::eval::{ElimCtx, EvalCtx};
use super::quote::QuoteCtx;
use super::unelab::UnelabCtx;
use super::unify::{PartialRenaming, UnifyCtx};
use super::{
    Decl, EnumDecl, EnumVariant, Expr, FunClosure, LetDecl, MetaSource, Module, NameSource, Pat,
    Value, VarName,
};
use crate::surface::pretty::PrettyCtx;
use crate::surface::Hole;
use crate::{surface, FileId};

pub struct ElabCtx {
    pub file: FileId,
    pub local_env: LocalEnv,
    pub meta_env: MetaEnv,
    pub item_env: ItemEnv,
    pub enum_env: EnumEnv,
    pub renaming: PartialRenaming,
    pub errors: Vec<ElabError>,
    name_source: NameSource,
}

impl ElabCtx {
    pub fn new(file: FileId) -> Self {
        Self {
            file,
            local_env: LocalEnv::new(),
            meta_env: MetaEnv::new(),
            item_env: ItemEnv::new(),
            enum_env: EnumEnv::new(),
            renaming: PartialRenaming::new(),
            errors: Vec::new(),
            name_source: NameSource::new(0),
        }
    }

    pub fn drain_errors(&mut self) -> impl Iterator<Item = ElabError> + '_ {
        self.errors.drain(..).chain(self.meta_env.errors())
    }

    pub fn eval_ctx(&mut self) -> EvalCtx {
        EvalCtx::new(
            &mut self.local_env.values,
            &self.item_env.values,
            &self.meta_env.values,
        )
    }

    pub fn elim_ctx(&self) -> ElimCtx { ElimCtx::new(&self.item_env.values, &self.meta_env.values) }

    pub fn quote_ctx(&mut self) -> QuoteCtx {
        QuoteCtx::new(
            self.local_env.values.len(),
            &self.item_env.values,
            &self.meta_env.values,
            &mut self.name_source,
        )
    }

    pub fn conv_ctx(&self) -> ConvCtx {
        ConvCtx::new(
            self.local_env.values.len(),
            &self.item_env.values,
            &self.meta_env.values,
        )
    }

    pub fn unelab_ctx(&mut self) -> UnelabCtx<'_> {
        UnelabCtx::new(
            &self.item_env.level_to_name,
            &self.enum_env.names,
            &self.meta_env.names,
            &mut self.local_env.names,
        )
    }

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

    fn push_meta_expr(&mut self, name: VarName, source: MetaSource, ty: Rc<Value>) -> Expr {
        let var = self.meta_env.push(name, source, ty);
        Expr::MetaInsertion(var, self.local_env.infos.clone())
    }

    fn push_meta_value(&mut self, name: VarName, source: MetaSource, ty: Rc<Value>) -> Rc<Value> {
        let expr = self.push_meta_expr(name, source, ty);
        self.eval_ctx().eval_expr(&expr)
    }

    fn unify_ctx(&mut self) -> UnifyCtx {
        UnifyCtx::new(
            &mut self.renaming,
            self.local_env.values.len(),
            &self.item_env.values,
            &mut self.meta_env.values,
            &mut self.name_source,
        )
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Refutability {
    Refutible,
    Irrefutible,
}

impl ElabCtx {
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    pub fn elab_module(&mut self, module: &surface::Module<TextRange>) -> Module {
        let surface::Module { decls } = module;
        let decls = decls.iter().map(|decl| self.elab_decl(decl)).collect();
        Module { decls }
    }

    fn elab_decl(&mut self, decl: &surface::Decl<TextRange>) -> Decl {
        match decl {
            surface::Decl::Error(_) => Decl::Error,
            surface::Decl::Let(_, decl) => Decl::Let(self.elab_let_decl(decl)),
            surface::Decl::Enum(_, decl) => Decl::Enum(self.elab_enum_decl(decl)),
        }
    }

    fn elab_let_decl(&mut self, decl: &surface::LetDecl<TextRange>) -> LetDecl {
        let surface::LetDecl { expr, name, ty } = decl;
        let (range, name) = name;

        let type_core = match ty {
            Some(ty) => self.check_expr_is_type(ty),
            None => {
                let source = MetaSource::LetDeclType(self.file, *range);
                let name = self.name_source.next();
                self.push_meta_expr(name, source, Rc::new(Value::Type))
            }
        };
        let type_value = &self.eval_ctx().eval_expr(&type_core);
        let expr_core = self.check_expr(expr, type_value);
        let expr_value = self.eval_ctx().eval_expr(&expr_core);
        let expr_value = self.elim_ctx().force_value(&expr_value);
        let expr_core = self.quote_ctx().quote_value(&expr_value);

        let type_value = &self.elim_ctx().force_value(type_value);
        let type_core = self.quote_ctx().quote_value(type_value);

        if let Some(name) = name {
            self.item_env
                .push(name.clone(), type_value.clone(), expr_value);
        }

        LetDecl {
            name: name.clone(),
            ty: Rc::new(type_core),
            expr: Rc::new(expr_core),
        }
    }

    fn elab_enum_decl(&mut self, decl: &surface::EnumDecl<TextRange>) -> EnumDecl {
        let surface::EnumDecl {
            name,
            args,
            variants,
        } = decl;

        let (_, name) = name;
        let initial_len = self.local_env.len();

        let (arg_names, args): (Vec<_>, Vec<_>) = args
            .iter()
            .map(|pat| {
                let (name, pat_type) = self.synth_simple_pat(pat);
                let type_core = self.quote_ctx().quote_value(&pat_type);
                self.local_env.push_param(name.clone(), pat_type);
                (name, type_core)
            })
            .unzip();
        let arg_names: Rc<[_]> = Rc::from(arg_names);
        let args: Rc<[_]> = Rc::from(args);

        let enum_type_id = self.enum_env.len().to_level();

        let variants = variants
            .iter()
            .map(|variant| {
                let surface::EnumVariant {
                    name,
                    args,
                    ret_type: ty,
                } = variant;

                let (_, name) = name;
                let initial_len = self.local_env.len();

                let (arg_names, args): (Vec<_>, Vec<_>) = args
                    .iter()
                    .map(|pat| {
                        let (name, pat_type) = self.synth_simple_pat(pat);
                        let type_core = self.quote_ctx().quote_value(&pat_type);
                        self.local_env.push_param(name.clone(), pat_type);
                        (name, type_core)
                    })
                    .unzip();

                let arg_names: Rc<[_]> = Rc::from(arg_names);
                let args: Rc<[_]> = Rc::from(args);

                let ret_type = match ty {
                    Some(ty) => {
                        let type_core = self.check_expr_is_type(ty);
                        let type_value = self.eval_ctx().eval_expr(&type_core);
                        let type_core = self.quote_ctx().quote_value(&type_value);
                        type_core
                    }
                    None if args.len() > 0 => {
                        Expr::FunCall(Rc::new(Expr::EnumType(enum_type_id)), args.clone())
                    }
                    None => Expr::EnumType(enum_type_id),
                };

                self.local_env.truncate(initial_len);

                EnumVariant {
                    name: name.clone(),
                    arg_names,
                    args,
                    ret_type: Rc::new(ret_type),
                }
            })
            .collect();

        self.local_env.truncate(initial_len);

        let decl = EnumDecl {
            name: name.clone(),
            arg_names: arg_names.clone(),
            args: args.clone(),
            variants,
        };

        let fun_type = if args.len() > 0 {
            Rc::new(Value::FunType(
                arg_names,
                FunClosure::new(self.local_env.values.clone(), args, Rc::new(Expr::Type)),
            ))
        } else {
            Rc::new(Value::Type)
        };

        self.enum_env.push(decl.clone(), fun_type);

        decl
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    pub fn synth_expr(&mut self, expr: &surface::Expr<TextRange>) -> (Expr, Rc<Value>) {
        self.synth_expr_inner(expr)
    }

    fn synth_error_expr(&mut self) -> (Expr, Rc<Value>) {
        let name = self.name_source.next();
        let ty = self.push_meta_value(name, MetaSource::Error, Rc::new(Value::Type));
        (Expr::Error, ty)
    }

    fn synth_expr_inner(&mut self, expr: &surface::Expr<TextRange>) -> (Expr, Rc<Value>) {
        match expr {
            surface::Expr::Error(_) => self.synth_error_expr(),
            surface::Expr::Hole(range, hole) => {
                let name = match hole {
                    Hole::Underscore => self.name_source.next(),
                    Hole::Name(name) => VarName::User(name.clone()),
                };
                let type_source = MetaSource::HoleType(self.file, *range);
                let expr_source = MetaSource::HoleExpr(self.file, *range);
                let fresh_name = self.name_source.next();
                let ty = self.push_meta_value(fresh_name, type_source, Rc::new(Value::Type));
                let expr = self.push_meta_expr(name, expr_source, ty.clone());
                (expr, ty)
            }
            surface::Expr::Name(range, name) => {
                if let Some((idx, ty)) = self.local_env.lookup(name) {
                    return (Expr::Local(idx), ty);
                }

                if let Some((level, ty)) = self.item_env.lookup(name) {
                    return (Expr::Item(level), ty);
                }

                match name.as_ref() {
                    "Type" => return (Expr::Type, Rc::new(Value::Type)),
                    "Bool" => return (Expr::BoolType, Rc::new(Value::Type)),
                    _ => {}
                }
                self.errors.push(ElabError::UnboundName {
                    file: self.file,
                    range: *range,
                    name: name.clone(),
                });
                self.synth_error_expr()
            }
            surface::Expr::Bool(_, b) => (Expr::Bool(*b), Rc::new(Value::BoolType)),
            surface::Expr::FunType(_, pats, ret) => {
                let initial_len = self.local_env.len();
                let (names, args): (Vec<_>, Vec<_>) = pats
                    .iter()
                    .map(|pat| {
                        let (name, pat_type) = self.synth_simple_pat(pat);
                        let type_core = self.quote_ctx().quote_value(&pat_type);
                        self.local_env.push_param(name.clone(), pat_type);
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
                let (names, args): (Vec<_>, Vec<_>) = pats
                    .iter()
                    .map(|pat| {
                        let (name, pat_type) = self.synth_simple_pat(pat);
                        let type_core = self.quote_ctx().quote_value(&pat_type);
                        self.local_env.push_param(name.clone(), pat_type);
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
            surface::Expr::FunCall(_, fun, args) => {
                let (fun_core, fun_type) = self.synth_expr(fun);
                let fun_type = self.elim_ctx().force_value(&fun_type);
                let closure = match fun_type.as_ref() {
                    Value::FunType(_, closure) => closure,
                    _ => {
                        let fun_type = self.pretty_value(&fun_type).into();
                        self.errors.push(ElabError::CallNonFun {
                            file: self.file,
                            range: fun.range(),
                            fun_type,
                        });
                        return self.synth_error_expr();
                    }
                };

                let expected_arity = closure.arity();
                let actual_arity = args.len();
                if actual_arity != expected_arity {
                    let fun_type = self.pretty_value(&fun_type).into();
                    self.errors.push(ElabError::ArityMismatch {
                        file: self.file,
                        range: fun.range(),
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
                let (name, pat_type) = self.synth_simple_pat(pat);
                let type_core = self.quote_ctx().quote_value(&pat_type);
                let init_core = self.check_expr(init, &pat_type);
                let init_value = self.eval_ctx().eval_expr(&init_core);

                self.local_env.push_def(name.clone(), init_value, pat_type);
                let (body_core, body_type) = self.synth_expr(body);
                self.local_env.pop();

                (
                    Expr::Let(
                        name,
                        Rc::new(type_core),
                        Rc::new(init_core),
                        Rc::new(body_core),
                    ),
                    body_type,
                )
            }
            surface::Expr::Match(range, scrut, arms) => {
                let name = self.name_source.next();
                let match_type = self.push_meta_value(
                    name,
                    MetaSource::MatchType(self.file, *range),
                    Rc::new(Value::Type),
                );
                (self.check_match_expr(scrut, arms, &match_type), match_type)
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

                let mut pats = pats.iter();
                while let Some((pat, (expected, cont))) =
                    Option::zip(pats.next(), self.elim_ctx().split_fun_closure(closure))
                {
                    dbg!(&expected);
                    let type_core = self.quote_ctx().quote_value(&expected);

                    let arg_value = Rc::new(Value::local(self.local_env.len().to_level()));
                    let name = self.check_simple_pat(pat, &expected);
                    self.local_env.push_param(name.clone(), expected);

                    closure = cont(arg_value.clone());
                    args_values.push(arg_value);
                    arg_types.push(type_core);
                    names.push(name);
                }

                let expected_ret = self.elim_ctx().call_closure(&initial_closure, args_values);
                let ret_core = self.check_expr(body, &expected_ret);
                self.local_env.truncate(initial_len);

                Expr::FunExpr(Rc::from(names), Rc::from(arg_types), Rc::new(ret_core))
            }
            (surface::Expr::Let(_, pat, init, body), _) => {
                let (name, pat_type) = self.synth_simple_pat(pat);
                let type_core = self.quote_ctx().quote_value(&pat_type);
                let init_core = self.check_expr(init, &pat_type);
                let init_value = self.eval_ctx().eval_expr(&init_core);

                self.local_env.push_def(name.clone(), init_value, pat_type);
                let body_core = self.check_expr(body, &expected);
                self.local_env.pop();

                Expr::Let(
                    name,
                    Rc::new(type_core),
                    Rc::new(init_core),
                    Rc::new(body_core),
                )
            }
            (surface::Expr::Match(_, scrut, arms), _) => {
                self.check_match_expr(scrut, arms, &expected)
            }

            // return early, rather than creating a fresh metavar and unifying it with the expected
            // type
            (surface::Expr::Error(_), _) => Expr::Error,
            _ => {
                let (core, got) = self.synth_expr(expr);
                match self.unify_ctx().unify_values(&got, &expected) {
                    Ok(()) => core,
                    Err(error) => {
                        let expected_type = self.pretty_value(&expected).into();
                        let actual_type = self.pretty_value(&got).into();
                        self.errors.push(ElabError::TypeMismatch {
                            file: self.file,
                            range: expr.range(),
                            expected_type,
                            actual_type,
                            error,
                        });
                        Expr::Error
                    }
                }
            }
        }
    }

    fn check_match_expr(
        &mut self,
        scrut: &surface::Expr<TextRange>,
        arms: &[(surface::Pat<TextRange>, surface::Expr<TextRange>)],
        expected: &Rc<Value>,
    ) -> Expr {
        // TODO: check for exhaustivity and report unreachable patterns

        // FIXME: update `expected` with defintions introduced by `check_match_pat`
        // without having to quote `expected` back to `Expr`

        let (scrut_core, scrut_type) = self.synth_expr(scrut);

        let expected_core = self.quote_ctx().quote_value(expected);
        let arms = arms
            .iter()
            .map(|(pat, expr)| {
                let initial_len = self.local_env.len();
                let pat_core = self.check_match_pat(pat, &scrut_type);
                let expected = &self.eval_ctx().eval_expr(&expected_core);
                let expr_core = self.check_expr(expr, expected);
                self.local_env.truncate(initial_len);
                (pat_core, expr_core)
            })
            .collect();
        Expr::Match(Rc::new(scrut_core), arms)
    }

    fn synth_simple_pat(&mut self, pat: &surface::SimplePat<TextRange>) -> (VarName, Rc<Value>) {
        let surface::SimplePat { name, ty } = pat;
        let (range, name) = name;
        let name = match name {
            Some(name) => VarName::User(name.clone()),
            None => VarName::Underscore,
        };
        let ty = match ty {
            Some(ty) => {
                let type_core = self.check_expr_is_type(ty);
                self.eval_ctx().eval_expr(&type_core)
            }
            None => {
                let name = self.name_source.next();
                self.push_meta_value(
                    name,
                    MetaSource::PatType(self.file, *range),
                    Rc::new(Value::Type),
                )
            }
        };
        (name, ty)
    }

    fn check_simple_pat(
        &mut self,
        pat: &surface::SimplePat<TextRange>,
        expected: &Rc<Value>,
    ) -> VarName {
        let surface::SimplePat { name, ty } = pat;
        let (range, name) = name;
        let name = match name {
            Some(name) => VarName::User(name.clone()),
            None => VarName::Underscore,
        };
        if let Some(ty) = ty {
            let type_core = self.check_expr_is_type(ty);
            let got = self.eval_ctx().eval_expr(&type_core);
            match self.unify_ctx().unify_values(&got, expected) {
                Ok(_) => {}
                Err(error) => {
                    let expected_type = self.pretty_value(expected).into();
                    let actual_type = self.pretty_value(&got).into();
                    self.errors.push(ElabError::TypeMismatch {
                        file: self.file,
                        range: *range,
                        expected_type,
                        actual_type,
                        error,
                    });
                }
            }
        }
        name
    }

    pub fn synth_match_pat(&mut self, pat: &surface::Pat<TextRange>) -> (Pat, Rc<Value>) {
        match pat {
            surface::Pat::Error(_) => {
                let name = self.name_source.next();
                let ty = self.push_meta_value(name, MetaSource::Error, Rc::new(Value::Type));
                (Pat::Error, ty)
            }
            surface::Pat::Wildcard(range) => {
                let name = self.name_source.next();
                let ty = self.push_meta_value(
                    name,
                    MetaSource::PatType(self.file, *range),
                    Rc::new(Value::Type),
                );
                self.local_env.push_param(VarName::Underscore, ty.clone());
                (Pat::Name(VarName::Underscore), ty)
            }
            surface::Pat::Name(range, name) => {
                let fresh_name = self.name_source.next();
                let ty = self.push_meta_value(
                    fresh_name,
                    MetaSource::PatType(self.file, *range),
                    Rc::new(Value::Type),
                );
                self.local_env
                    .push_param(VarName::User(name.clone()), ty.clone());
                (Pat::Name(VarName::User(name.clone())), ty)
            }
            surface::Pat::Bool(_, b) => {
                let name = self.name_source.next();
                let value = Rc::new(Value::Bool(*b));
                let ty = Rc::new(Value::BoolType);
                self.local_env.push_def(name, value, ty);
                (Pat::Bool(*b), Rc::new(Value::BoolType))
            }
            surface::Pat::Ann(_, pat, ty) => {
                let type_core = self.check_expr_is_type(ty);
                let type_value = self.eval_ctx().eval_expr(&type_core);
                let pat_core = self.check_match_pat(pat, &type_value);
                (pat_core, type_value)
            }
        }
    }

    pub fn check_match_pat(&mut self, pat: &surface::Pat<TextRange>, expected: &Rc<Value>) -> Pat {
        let expected = self.elim_ctx().force_value(expected);
        match pat {
            surface::Pat::Error(_) => Pat::Error,
            surface::Pat::Wildcard(_) => {
                self.local_env.push_param(VarName::Underscore, expected);
                Pat::Name(VarName::Underscore)
            }
            surface::Pat::Name(_, name) => {
                self.local_env
                    .push_param(VarName::User(name.clone()), expected);
                Pat::Name(VarName::User(name.clone()))
            }
            _ => {
                let (core, got) = self.synth_match_pat(pat);
                match self.unify_ctx().unify_values(&got, &expected) {
                    Ok(_) => core,
                    Err(error) => {
                        let expected_type = self.pretty_value(&expected).into();
                        let actual_type = self.pretty_value(&got).into();
                        self.errors.push(ElabError::TypeMismatch {
                            file: self.file,
                            range: pat.range(),
                            expected_type,
                            actual_type,
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
                (None, MetaSource::HoleType(..)) => None,
                (None, MetaSource::Error) => None,
                (None, source) => Some(ElabError::UnsolvedMeta { source: *source }),
            })
    }
}
