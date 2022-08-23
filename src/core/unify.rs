use std::rc::Rc;

use contracts::debug_ensures;

use super::env::{EnvLen, SharedEnv, UniqueEnv, VarIndex, VarLevel};
use super::eval::ElimCtx;
use super::{Elim, Expr, FunClosure, Head, Value};

pub struct UnifyCtx<'env> {
    renaming: &'env mut PartialRenaming,
    local_env: EnvLen,
    meta_values: &'env mut UniqueEnv<Option<Rc<Value>>>,
}

impl<'env> UnifyCtx<'env> {
    pub fn new(
        renaming: &'env mut PartialRenaming,
        local_env: EnvLen,
        meta_values: &'env mut UniqueEnv<Option<Rc<Value>>>,
    ) -> Self {
        Self {
            renaming,
            local_env,
            meta_values,
        }
    }

    fn elim_ctx(&self) -> ElimCtx<'_> { ElimCtx::new(self.meta_values) }

    #[debug_ensures(self.local_env == old(self.local_env))]
    pub fn unify_values(
        &mut self,
        value1: &Rc<Value>,
        value2: &Rc<Value>,
    ) -> Result<(), UnifyError> {
        if Rc::ptr_eq(value1, value2) {
            return Ok(());
        }

        let value1 = self.elim_ctx().force_value(value1);
        let value2 = self.elim_ctx().force_value(value2);

        match (value1.as_ref(), value2.as_ref()) {
            (Value::Error, _) | (_, Value::Error) => Ok(()),

            (Value::Type, Value::Type) => Ok(()),
            (Value::BoolType, Value::BoolType) => Ok(()),
            (Value::Bool(b1), Value::Bool(b2)) if b1 == b2 => Ok(()),

            (Value::Stuck(Head::Meta(var1), spine1), _) => self.solve(*var1, spine1, &value2),
            (_, Value::Stuck(Head::Meta(var2), spine2)) => self.solve(*var2, spine2, &value1),

            (Value::Stuck(head1, spine1), Value::Stuck(head2, spine2)) if head1 == head2 => {
                self.unify_spines(spine1, spine2)
            }

            (Value::FunValue(_, closure1), Value::FunValue(_, closure2)) => {
                self.unify_fun_closures(closure1, closure2)
            }
            (Value::FunValue(_, closure1), _) => self.unify_fun_values(closure1, value2.clone()),
            (_, Value::FunValue(_, closure2)) => self.unify_fun_values(closure2, value1.clone()),

            (Value::FunType(_, closure1), Value::FunType(_, closure2)) => {
                self.unify_fun_closures(closure1, closure2)
            }

            _ => Err(UnifyError::Mismatch),
        }
    }

    #[debug_ensures(self.local_env == old(self.local_env))]
    fn unify_fun_closures(
        &mut self,
        closure1: &FunClosure,
        closure2: &FunClosure,
    ) -> Result<(), UnifyError> {
        if closure1.arity() != closure2.arity() {
            return Err(UnifyError::Mismatch);
        }

        let initial_len = self.local_env;
        let mut args = Vec::with_capacity(closure1.arity());

        let mut closure1 = closure1.clone();
        let mut closure2 = closure2.clone();

        while let Some(((arg1, cont1), (arg2, cont2))) = Option::zip(
            self.elim_ctx().split_fun_closure(closure1.clone()),
            self.elim_ctx().split_fun_closure(closure2.clone()),
        ) {
            match self.unify_values(&arg1, &arg2) {
                Ok(_) => {}
                Err(err) => {
                    self.local_env.truncate(initial_len);
                    return Err(err);
                }
            }

            let arg = Rc::new(Value::local(self.local_env.to_level()));
            closure1 = cont1(arg.clone());
            closure2 = cont2(arg.clone());
            self.local_env.push();
            args.push(arg);
        }

        let body1 = self.elim_ctx().call_closure(&closure1, args.clone());
        let body2 = self.elim_ctx().call_closure(&closure2, args);
        let result = self.unify_values(&body1, &body2);

        self.local_env.truncate(initial_len);
        result
    }

    #[debug_ensures(self.local_env == old(self.local_env))]
    fn unify_fun_values(
        &mut self,
        lhs_closure: &FunClosure,
        rhs_value: Rc<Value>,
    ) -> Result<(), UnifyError> {
        let initial_len = self.local_env;
        let args: Vec<_> = (0..lhs_closure.arity())
            .map(|_| Rc::new(Value::local(self.local_env.push().to_level())))
            .collect();
        self.local_env.truncate(initial_len);

        let value1 = self.elim_ctx().call_fun(rhs_value, args.clone());
        let value2 = self.elim_ctx().call_closure(lhs_closure, args);
        self.local_env.extend(EnvLen(lhs_closure.arity()));
        let result = self.unify_values(&value1, &value2);
        self.local_env.truncate(initial_len);
        result
    }

    #[debug_ensures(self.local_env == old(self.local_env))]
    fn unify_spines(&mut self, spine1: &[Elim], spine2: &[Elim]) -> Result<(), UnifyError> {
        if spine1.len() != spine2.len() {
            return Err(UnifyError::Mismatch);
        }

        for (elim1, elim2) in spine1.iter().zip(spine2.iter()) {
            match (elim1, elim2) {
                (Elim::FunCall(args1), Elim::FunCall(args2)) => self.unify_args(args1, args2)?,
            }
        }
        Ok(())
    }

    #[debug_ensures(self.local_env == old(self.local_env))]
    fn unify_args(&mut self, args1: &[Rc<Value>], args2: &[Rc<Value>]) -> Result<(), UnifyError> {
        if args1.len() != args2.len() {
            return Err(UnifyError::Mismatch);
        }

        for (arg1, arg2) in args1.iter().zip(args2.iter()) {
            self.unify_values(arg1, arg2)?;
        }
        Ok(())
    }

    /// Solve a pattern unification problem that looks like:
    ///
    /// ```text
    /// ?α spine =? value`
    /// ```
    ///
    /// If successful, the flexible environment will be updated with a solution
    /// that looks something like:
    ///
    /// ```text
    /// ?α := fn spine => value
    /// ```
    fn solve(
        &mut self,
        meta_var: VarLevel,
        spine: &[Elim],
        value: &Rc<Value>,
    ) -> Result<(), UnifyError> {
        self.init_renaming(spine)?;
        let term = self.rename_value(meta_var, value)?;
        let fun_expr = self.fun_intros(spine, term);
        let solution = self
            .elim_ctx()
            .eval_ctx(&mut SharedEnv::new())
            .eval_expr(&fun_expr);

        self.meta_values.set_by_level(meta_var, Some(solution));

        Ok(())
    }

    /// Re-initialise the [`UnifyContext::renaming`] by mapping the rigid
    /// variables in the spine to the rigid variables in the solution. This
    /// can fail if the spine does not contain distinct rigid variables.
    fn init_renaming(&mut self, spine: &[Elim]) -> Result<(), SpineError> {
        self.renaming.init(self.local_env);

        for elim in spine {
            match elim {
                Elim::FunCall(args) => {
                    for arg in args {
                        match self.elim_ctx().force_value(arg).as_ref() {
                            Value::Stuck(Head::Local(source_var), spine)
                                if spine.is_empty() && self.renaming.set_local(*source_var) => {}
                            Value::Stuck(Head::Local(source_var), _) => {
                                return Err(SpineError::NonLinearSpine(*source_var))
                            }
                            _ => return Err(SpineError::NonRigidFunApp),
                        }
                    }
                }
            }
        }
        Ok(())
    }

    /// Rename `value` to a [`Expr`], while at the same time using the current
    /// renaming to update local variables, failing if the partial renaming is
    /// not defined (resulting in an [scope error][Error::ScopeError]), and also
    /// checking for occurrences of the `flexible_var` (resulting in an [occurs
    /// check error][Error::InfiniteSolution]).
    ///
    /// This allows us to subsequently wrap the returned term in function
    /// literals, using [`Context::function_intros`].
    fn rename_value(&mut self, meta_var: VarLevel, value: &Rc<Value>) -> Result<Expr, RenameError> {
        match self.elim_ctx().force_value(value).as_ref() {
            Value::Error => Ok(Expr::Error),
            Value::Type => Ok(Expr::Type),
            Value::BoolType => Ok(Expr::BoolType),
            Value::Bool(b) => Ok(Expr::Bool(*b)),
            Value::Stuck(head, spine) => {
                let head = match head {
                    Head::Local(source_var) => match self.renaming.get_as_local(*source_var) {
                        None => return Err(RenameError::EscapingRigidVar(*source_var)),
                        Some(target_var) => Expr::Local(target_var),
                    },
                    Head::Meta(var) => {
                        if *var == meta_var {
                            return Err(RenameError::InfiniteSolution);
                        } else {
                            Expr::Meta(*var)
                        }
                    }
                };
                spine.iter().fold(Ok(head), |head, elim| {
                    Ok(match elim {
                        Elim::FunCall(args) => {
                            let args: Rc<[Expr]> = args
                                .iter()
                                .map(|arg| self.rename_value(meta_var, arg))
                                .collect::<Result<_, _>>()?;
                            Expr::FunCall(Rc::new(head?), args)
                        }
                    })
                })
            }
            Value::FunType(names, closure) => {
                let (args, ret) = self.rename_fun_closure(meta_var, closure)?;
                Ok(Expr::FunType(names.clone(), args, Rc::new(ret)))
            }
            Value::FunValue(names, closure) => {
                let (args, ret) = self.rename_fun_closure(meta_var, closure)?;
                Ok(Expr::FunExpr(names.clone(), args, Rc::new(ret)))
            }
        }
    }

    fn fun_intros(&self, spine: &[Elim], expr: Expr) -> Expr {
        spine.iter().fold(expr, |expr, elim| match elim {
            Elim::FunCall(args) => {
                let arity = args.len();
                let names = Rc::from(vec![None; arity]);
                let types = Rc::from(vec![Expr::Error; arity]); // TODO: what should the introduced type be?
                Expr::FunExpr(names, types, Rc::new(expr))
            }
        })
    }

    fn rename_fun_closure(
        &mut self,
        meta_var: VarLevel,
        closure: &FunClosure,
    ) -> Result<(Rc<[Expr]>, Expr), RenameError> {
        let initial_source_len = self.renaming.source.len();
        let initial_target_len = self.renaming.target;

        let initial_closure = closure.clone();
        let mut closure = closure.clone();
        let mut exprs = Vec::with_capacity(closure.arity());
        let mut args = Vec::with_capacity(closure.arity());

        while let Some((value, cont)) = self.elim_ctx().split_fun_closure(closure.clone()) {
            let arg = self.renaming.next_local_var();
            closure = cont(arg.clone());
            let expr = match self.rename_value(meta_var, &value) {
                Ok(expr) => expr,
                Err(err) => {
                    self.renaming
                        .truncate(initial_source_len, initial_target_len);
                    return Err(err);
                }
            };
            exprs.push(expr);
            args.push(arg);
            self.renaming.push_local();
        }

        let body = self.elim_ctx().call_closure(&initial_closure, args);
        let body = self.rename_value(meta_var, &body);

        self.renaming
            .truncate(initial_source_len, initial_target_len);

        Ok((Rc::from(exprs), body?))
    }
}

pub struct PartialRenaming {
    /// Mapping from rigid variables in the source environment to rigid
    /// variables in the target environment.
    source: UniqueEnv<Option<VarLevel>>,
    /// The length of the target binding environment
    target: EnvLen,
}

impl PartialRenaming {
    pub fn new() -> Self {
        Self {
            source: UniqueEnv::new(),
            target: EnvLen::default(),
        }
    }

    /// Re-initialise the renaming to the requested `source_len`, reusing the
    /// previous allocation.
    fn init(&mut self, source_len: EnvLen) {
        self.source.clear();
        self.source.resize_with(source_len, || None);
        self.target.clear();
    }

    fn next_local_var(&self) -> Rc<Value> { Rc::new(Value::local(self.source.len().to_level())) }

    /// Set a rigid source variable to rigid target variable mapping, ensuring
    /// that the variable appears uniquely.
    ///
    /// # Returns
    ///
    /// - `true` if the rigid binding was set successfully.
    /// - `false` if the rigid binding was already set.
    fn set_local(&mut self, source_var: VarLevel) -> bool {
        let is_unique = self.get_as_global(source_var).is_none();

        if is_unique {
            let target_var = Some(self.target.to_level());
            self.source.set_by_level(source_var, target_var);
            self.target.push();
        }

        is_unique
    }

    /// Push an extra local binding onto the renaming.
    fn push_local(&mut self) {
        let target_var = self.target.to_level();
        self.source.push(Some(target_var));
        self.target.push();
    }

    /// Pop a local binding off the renaming.
    fn pop_local(&mut self) {
        self.source.pop();
        self.target.pop();
    }

    /// Get the local variable in the target environment that will be used in
    /// place of the `source_var`.
    fn get_as_global(&self, source_var: VarLevel) -> Option<VarLevel> {
        self.source.get_by_level(source_var).copied().flatten()
    }

    /// Rename a local variable in the source environment to a rigid variable in
    /// the target environment.
    fn get_as_local(&self, source_var: VarLevel) -> Option<VarIndex> {
        let target_var = self.get_as_global(source_var)?;
        Some(self.target.level_to_index(target_var).unwrap())
    }

    fn truncate(&mut self, source: EnvLen, target: EnvLen) {
        self.source.truncate(source);
        self.target.truncate(target);
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnifyError {
    Mismatch,
    Spine(SpineError),
    Rename(RenameError),
}

impl From<SpineError> for UnifyError {
    fn from(other: SpineError) -> Self { Self::Spine(other) }
}

impl From<RenameError> for UnifyError {
    fn from(other: RenameError) -> Self { Self::Rename(other) }
}

/// An error that was found in the problem spine.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SpineError {
    /// A rigid variable appeared multiple times in a flexible spine.
    ///
    /// For example:
    ///
    /// ```text
    /// ?α x x =? x`
    /// ```
    ///
    /// This results in two distinct solutions:
    ///
    /// - `?α := fun x _ => x`
    /// - `?α := fun _ x => x`
    ///
    /// We only want unification to result in a unique solution, so we fail
    /// to unify in this case.
    ///
    /// Another example, assuming `true : Bool`, is:
    ///
    /// ```text
    /// ?α true =? true
    /// ```
    ///
    /// This also has multiple solutions, for example:
    ///
    /// - `?α := fun _ => true`
    /// - `?α := fun x => x`
    /// - `?α := fun x => if x then true else false`
    ///
    /// It's also possible that the return type of `?α` is not always `Bool`,
    /// for example:
    ///
    /// ```text
    /// ?α : fun (b : Bool) -> if b then Bool else (Bool -> Bool)
    /// ```
    ///
    /// In this case the example solution `?α := fun _ => true` is not even
    /// well-typed! In contrast, if the flexible spine only has distinct rigid
    /// variables, even if the return type is dependent, rigid variables block
    /// all computation in the return type, and the pattern solution is
    /// guaranteed to be well-typed.
    NonLinearSpine(VarLevel),
    /// A flexible variable was found in the problem spine.
    NonRigidFunApp,
}

/// An error that occurred when renaming the solution.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RenameError {
    /// A free rigid variable in the compared value does not occur in the
    /// flexible spine.
    ///
    /// For example, where `z : U` is a rigid variable:
    ///
    /// ```text
    /// ?α x y =? z -> z
    /// ```
    ///
    /// There is no solution for this flexible variable because `?α` is the
    /// topmost-level scope, so it can only abstract over `x` and `y`, but
    /// these don't occur in `z -> z`.
    EscapingRigidVar(VarLevel),
    /// The flexible variable occurs in the value being compared against.
    /// This is sometimes referred to as an 'occurs check' failure.
    ///
    /// For example:
    ///
    /// ```text
    /// ?α =? ?α -> ?α
    /// ```
    ///
    /// Here `?α` occurs in the right hand side, so in order to solve this
    /// flexible variable we would end up going into an infinite loop,
    /// attempting to construct larger and larger solutions:
    ///
    /// - `?α =? ?α -> ?α`
    /// - `?α =? (?α -> ?α) -> (?α -> ?α)`
    /// - `?α =? ((?α -> ?α) -> (?α -> ?α)) -> ((?α -> ?α) -> (?α -> ?α))`
    /// - etc.
    InfiniteSolution,
}
