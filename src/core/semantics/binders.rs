use crate::core::env::EnvLen;
use crate::core::syntax::*;

pub trait IsClosed<LocalEnv = EnvLen, MetaEnv = EnvLen> {
    /// Returns `true` if `self` is "closed" (does not contain free local or
    /// meta variables) with respect to `local_env` and `meta_env`.
    fn is_closed(&self, local_env: LocalEnv, meta_env: MetaEnv) -> bool;
}

impl IsClosed for Expr {
    fn is_closed(&self, mut local_env: EnvLen, meta_env: EnvLen) -> bool {
        match self {
            Self::Prim(_) | Self::Lit(_) | Self::Global(_) => true,
            Self::Local(var) => var.0 < local_env.0,
            Self::Meta(var) | Self::MetaInsertion(var, ..) => var.0 < meta_env.0,
            Self::FunType(args, ret) | Self::FunExpr(args, ret) => {
                args.iter().all(|FunArg { pat, r#type }| {
                    let ret = r#type.is_closed(local_env, meta_env);
                    local_env.subst_pat(pat);
                    ret
                }) && ret.is_closed(local_env, meta_env)
            }
            Self::FunCall(fun, args) => {
                fun.is_closed(local_env, meta_env)
                    && args.iter().all(|arg| arg.is_closed(local_env, meta_env))
            }
            Self::Let(pat, r#type, init, body) => {
                r#type.is_closed(local_env, meta_env)
                    && init.is_closed(local_env, meta_env)
                    && body.is_closed(local_env + pat.num_binders(), meta_env)
            }
            Self::Match(scrut, branches) => {
                scrut.is_closed(local_env, meta_env)
                    && branches
                        .iter()
                        .all(|(pat, expr)| expr.is_closed(local_env + pat.num_binders(), meta_env))
            }
        }
    }
}

impl IsClosed for Value {
    fn is_closed(&self, local_env: EnvLen, meta_env: EnvLen) -> bool {
        match self {
            Self::Lit(_) => true,
            Self::Stuck(head, spine) => {
                let head = match head {
                    Head::Prim(_) | Head::Global(_) => true,
                    Head::Local(var) => var.0 < local_env.0,
                    Head::Meta(var) => var.0 < meta_env.0,
                };
                head && spine.iter().all(|elim| match elim {
                    Elim::FunCall(args) => {
                        args.iter().all(|arg| arg.is_closed(local_env, meta_env))
                    }
                    Elim::Match(closure) => closure.is_closed((), meta_env),
                })
            }
            Self::FunType(closure) | Self::FunValue(closure) => closure.is_closed((), meta_env),
        }
    }
}

impl IsClosed<()> for FunClosure {
    fn is_closed(&self, _: (), meta_env: EnvLen) -> bool {
        let mut local_env = self.env.len();
        self.args.is_closed(&mut local_env, meta_env) && self.body.is_closed(local_env, meta_env)
    }
}

impl IsClosed<()> for MatchClosure {
    fn is_closed(&self, _: (), meta_env: EnvLen) -> bool {
        let mut local_env = self.env.len();
        self.branches.iter().all(|(pat, expr)| {
            let initial_len = local_env;
            local_env.subst_pat(pat);
            local_env.truncate(initial_len);
            expr.is_closed(local_env, meta_env)
        })
    }
}

impl<Type: IsClosed> IsClosed<&mut EnvLen> for Telescope<Type> {
    fn is_closed(&self, mut local_env: &mut EnvLen, meta_env: EnvLen) -> bool {
        self.0.iter().all(|FunArg { pat, r#type }| {
            let ret = r#type.is_closed(*local_env, meta_env);
            local_env.subst_pat(pat);
            ret
        })
    }
}
