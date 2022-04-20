use std::collections::BTreeMap;
use std::sync::Arc;

use crate::ast::{Expr, ExprId, IdentId, Type, TypeId, ExprNode, Span, FnId, ModId};

#[salsa::query_group(TypeCheckStorage)]
pub trait TypeCheck: crate::ast::AstQuery + crate::parser::ParserQuery {
    fn type_check(&self, mod_id: crate::SourceId) -> Result<ModId, crate::Error>;

    fn check_mod(&self, mod_id: ModId) -> Result<(), Vec<TypeError>>;
    fn check_fn(&self, fn_id: FnId) -> Result<(), Vec<TypeError>>;
    fn check(
        &self,
        env: Arc<BTreeMap<IdentId, TypeId>>,
        expr: ExprId,
        ty: TypeId,
    ) -> Result<(), Vec<TypeError>>;
    fn infer(&self, env: Arc<BTreeMap<IdentId, TypeId>>, expr: ExprId) -> Option<TypeId>;


    fn fn_ty_env(&self, fn_id: FnId) -> Arc<BTreeMap<IdentId, TypeId>>;
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum TypeError {
    Expected { typ: TypeId, expected: TypeId },
    UndefinedVar { ident: IdentId },
    AnnotationNeeded,
    TooFewParams(Vec<TypeId>),
    TooManyParams(Vec<ExprNode<Span>>),
}

pub fn type_check(db: &dyn TypeCheck, src_id: crate::SourceId) -> Result<ModId, crate::Error> {
    let mod_id = db.parse(src_id);
    // TODO this is kinda whack, maybe return like a typed module?
    db.check_mod(mod_id).map_err(crate::Error::TypeCheck)?;
    Ok(mod_id)
}

fn fn_ty_env(db: &dyn TypeCheck, fn_id: FnId) -> Arc<BTreeMap<IdentId, TypeId>> {
    let params = db.fn_params(fn_id);
    Arc::new(params.into_iter()
        .map(|(ident_id, ty)| (ident_id, ty.kind))
        .collect())
}

pub fn check_mod(db: &dyn TypeCheck, mod_id: ModId) -> Result<(), Vec<TypeError>> {
    let mut errs = vec![];
    for fun in db.mod_fns(mod_id) {
        if let Err(e) = db.check_fn(fun.kind) {
            errs.extend(e);
        }
    }
    if !errs.is_empty() {
        return Err(errs);
    }
    Ok(())
}

pub fn check_fn(db: &dyn TypeCheck, fn_id: FnId) -> Result<(), Vec<TypeError>> {
    let ret = db.fn_ret(fn_id);
    let env = db.fn_ty_env(fn_id);
    db.check(env, db.fn_body(fn_id).kind, ret.kind)
}

pub fn check(
    db: &dyn TypeCheck,
    env: Arc<BTreeMap<IdentId, TypeId>>,
    expr: ExprId,
    ty: TypeId,
) -> Result<(), Vec<TypeError>> {
    match db.lookup_intern_expr_data(expr) {
        Expr::Error => panic!("Whoops"),
        Expr::Li32(_) => {
            let typ = db.lookup_intern_type_data(ty);
            if typ == Type::Ti32 {
                Ok(())
            } else {
                Err(vec![TypeError::Expected { 
                    typ: db.intern_type_data(Type::Ti32), 
                    expected: ty 
                }])
            }
        },
        Expr::LStr(_) => todo!(),
        Expr::Var(i) => env
            .get(&i)
            .ok_or(vec![TypeError::UndefinedVar { ident: i }])
            .and_then(|var_ty| {
                if var_ty == &ty {
                    Ok(())
                } else {
                    Err(vec![TypeError::Expected {
                        typ: *var_ty,
                        expected: ty,
                    }])
                }
            }),
        Expr::Sexpr(exprs) => {
            let head = exprs[0].kind;
            let spine = &exprs[1..];
            db.infer(env.clone(), head)
                .ok_or(vec![TypeError::AnnotationNeeded])
                .and_then(|head_ty| {
                    match db.lookup_intern_type_data(head_ty) {
                        Type::TFun { params, ret } => Ok((params, ret)),
                        // TODO: Improve this diagnostic
                        _ => Err(vec![TypeError::Expected {
                            typ: head_ty,
                            expected: db.intern_type_data(Type::TFun {
                                params: vec![db.intern_type_data(Type::THole)],
                                ret: ty,
                            }),
                        }]),
                    }
                })
                .and_then(|(params, ret)| {
                    let mut errs = vec![];
                    if ret != ty {
                        errs.push(TypeError::Expected { typ: ret, expected: ty });
                    }
                    if params.len() > spine.len() {
                        errs.push(TypeError::TooFewParams((&params[spine.len()..]).to_owned()));
                        return Err(errs);
                    }
                    if params.len() < spine.len() {
                        errs.push(TypeError::TooManyParams((&spine[params.len()..]).to_owned()));
                        return Err(errs);
                    }
                    // TODO: Error reporting on all or some
                    for (param, expr) in params.into_iter().zip(spine.iter().map(|s| s.kind)) {
                        if let Err(e) = db.check(env.clone(), expr, param) {
                            errs.extend(e);
                        }
                    }
                    if !errs.is_empty() {
                        return Err(errs);
                    }
                    Ok(())
                })
        }
    }
}

pub fn infer(
    db: &dyn TypeCheck,
    env: Arc<BTreeMap<IdentId, TypeId>>,
    expr: ExprId,
) -> Option<TypeId> {
    match db.lookup_intern_expr_data(expr) {
        Expr::Error => panic!("Whoops"),
        Expr::Li32(_) => {
            let ty_id = db.intern_type_data(Type::Ti32);
            Some(ty_id)
        }
        Expr::LStr(_) => todo!(),
        Expr::Var(i) => env.get(&i).cloned(),
        Expr::Sexpr(exprs) =>
        // My method of parsing types doesn't really work here
        // Need to decide if I want to go all in on currying or
        // go for a more traditional route
        {
            todo!()
        }
    }
}
