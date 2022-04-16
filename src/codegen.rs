use crate::{ast::{self, AstQuery}, parser::ParserQuery};
use parity_wasm::builder::{CodeLocation, FunctionBuilder, ModuleBuilder, Signature, SignatureBuilder};
use parity_wasm::elements::{FuncBody, Instruction, Instructions, ValueType};
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct WasmModule(Arc<parity_wasm::elements::Module>);
impl PartialEq for WasmModule {
    fn eq(&self, other: &WasmModule) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}
impl Eq for WasmModule {}

// A simple wrapper to make wasm types Clone and Eq
#[derive(Debug)]
pub struct Ref<T>(Arc<T>);
impl<T> Ref<T> {
    pub fn new(val: T) -> Self {
        Self(Arc::new(val))
    }
}
impl<T> Clone for Ref<T> {
    fn clone(&self) -> Self {
        Ref(self.0.clone())
    }
}
impl<T> PartialEq for Ref<T> {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}
impl<T> Eq for Ref<T> {}

#[salsa::query_group(CodegenStorage)]
pub trait Codegen: AstQuery + ParserQuery {
    fn codegen(&self, src_id: crate::SourceId) -> parity_wasm::elements::Module;

    fn codegen_mod(&self, mod_id: ast::ModId) -> parity_wasm::elements::Module;

    #[salsa::input]
    fn fn_codeloc(&self, mod_id: ast::ModId, ident_id: ast::IdentId) -> CodeLocation;

    fn mod_fns_signatures(&self, mod_id: ast::ModId) -> Vec<Signature>;
    fn mod_names(&self, mod_id: ast::ModId) -> parity_wasm::elements::NameSection;
    fn expr_primop(&self, expr_id: ast::ExprId) -> Option<Instruction>;
    fn fn_params_wasm_ty(&self, fn_id: ast::FnId) -> Vec<ValueType>;
    fn fn_ret_wasm_ty(&self, fn_id: ast::FnId) -> ValueType;

    // Converts a type to the wasm type that is used to pass it as a param
    // Notably this means any complicated type is passed by reference as an i32
    fn ty_param_type(&self, ty_id: ast::TypeId) -> ValueType;
}

// TODO: Is there any value in this method? Should I just present the SourceId -> Module interface with out the extra of ModId -> Module?
pub fn codegen(db: &dyn Codegen, src_id: crate::SourceId) -> parity_wasm::elements::Module {
    let mod_id = db.parse(src_id);
    db.codegen_mod(mod_id)
}

pub type CallEnv = HashMap<ast::IdentId, u32>;
fn codegen_mod(db: &dyn Codegen, mod_id: ast::ModId) -> parity_wasm::elements::Module {
    let mut module = ModuleBuilder::new();
    let sigs = db.mod_fns_signatures(mod_id);
    let calls = module.push_signatures(sigs.clone());
    // Mapping from fn name to it's call id in this module, used for methods calls within this module's function bodies
    let env: CallEnv = db.mod_fns(mod_id).iter().zip(calls)
        .map(|(id, call)| (db.fn_name(*id), call)).collect();

    for (fn_id, sig) in db.mod_fns(mod_id).into_iter().zip(sigs) {
        module.push_function(codegen_fn(
            db,
            &env,
            sig,
            fn_id));
    }

    module
        .with_section(parity_wasm::elements::Section::Name(db.mod_names(mod_id)))
        .build()
}

fn mod_names(db: &dyn Codegen, mod_id: ast::ModId) -> parity_wasm::elements::NameSection {
    let mut fns = parity_wasm::elements::FunctionNameSubsection::default();
    let mut locals = parity_wasm::elements::LocalNameSubsection::default();
    // This is precarious but should work?
    for (fn_id, i) in db.mod_fns(mod_id).into_iter().zip(0u32..) {
        fns.names_mut().insert(i, db.lookup_intern_ident_data(db.fn_name(fn_id)).to_string());

        let local_map = db.fn_local_vars(fn_id).into_iter().zip(0u32..)
            .map(|(id, idx)| (idx, db.lookup_intern_ident_data(id).to_string()))
            .collect::<parity_wasm::elements::NameMap>();
        locals.local_names_mut().insert(i, local_map);
    }

    parity_wasm::elements::NameSection::new(
        None,
        Some(fns),
        Some(locals)
    )
}

fn mod_fns_signatures(db: &dyn Codegen, mod_id: ast::ModId) -> Vec<Signature> {
    db.mod_fns(mod_id).into_iter()
        .map(|fn_id| SignatureBuilder::new()
            .with_params(db.fn_params_wasm_ty(fn_id))
            .with_result(db.fn_ret_wasm_ty(fn_id))
            .build_sig())
        .collect()
}

fn codegen_fn(
    db: &dyn Codegen,
    env: &CallEnv,
    sig: Signature,
    fn_id: ast::FnId,
) -> parity_wasm::builder::FunctionDefinition {
    FunctionBuilder::new()
        .with_signature(sig)
        .with_body(codegen_fn_body(db, env, fn_id))
        .build()
}

fn codegen_fn_body(
    db: &dyn Codegen, 
    env: &CallEnv,
    fn_id: ast::FnId) -> FuncBody {
    let body_id = db.fn_body(fn_id);

    let local_ids = db.fn_local_vars(fn_id);
    let locals = parity_wasm::elements::Local::new(local_ids.len() as u32, ValueType::I32);

    let local_env = local_ids.into_iter().zip(0u32..).collect::<HashMap<_, _>>();

    let mut ins = body_instructions(db, env, &local_env, body_id);
    ins.push(Instruction::End);
    FuncBody::new(vec![locals], Instructions::new(ins))
}

fn body_instructions(
    db: &dyn Codegen,
    env: &CallEnv,
    local_env: &HashMap<ast::IdentId, u32>,
    expr_id: ast::ExprId,
) -> Vec<Instruction> {
    match db.expr_kind(expr_id) {
        ast::Expr::Error => panic!("Whoops"),
        ast::Expr::Li32(i) => vec![Instruction::I32Const(i)],
        ast::Expr::LStr(_) => todo!(),
        ast::Expr::Var(id) => match local_env.get(&id) {
            Some(local) => vec![Instruction::GetLocal(*local)],
            None => {
                eprintln!("looing for {:?} in\n{:?}", id, local_env);
                panic!("Undefined local")
            }
        },
        ast::Expr::Sexpr(exprs) => {
            let func = exprs[0];
            match db.expr_kind(func) {
                ast::Expr::Var(id) => {
                    let idx = env[&id];
                    exprs[1..]
                        .iter()
                        .rev()
                        .flat_map(|id| body_instructions(db, env, local_env, *id).into_iter())
                        .chain(std::iter::once(Instruction::Call(idx)))
                        .collect()
                }
                _ => panic!("Don't know how to call this"),
            }
            //TODO: Need to handle recursion
        }
    }
}

fn expr_primop(
    db: &dyn Codegen,
    expr_id: ast::ExprId,
) -> Option<parity_wasm::elements::Instruction> {
    match db.expr_kind(expr_id) {
        ast::Expr::Var(id) => db.ident_primtive(id),
        _ => None,
    }
}

fn fn_params_wasm_ty(db: &dyn Codegen, fn_id: ast::FnId) -> Vec<ValueType> {
    db.fn_params(fn_id)
        .into_iter()
        .map(|(_, ty)| db.ty_param_type(ty))
        .collect()
}
fn fn_ret_wasm_ty(db: &dyn Codegen, fn_id: ast::FnId) -> ValueType {
    db.ty_param_type(db.fn_ret(fn_id))
}

fn ty_param_type(db: &dyn Codegen, ty_id: ast::TypeId) -> ValueType {
    let ty = db.lookup_intern_type_node(ty_id);
    match ty.kind {
        ast::Type::THole => panic!("Shouldn't have a hole here"),
        ast::Type::Ti32 => ValueType::I32,
        // Functions are passed as a pointer
        ast::Type::TFun(_, _) => ValueType::I32,
    }
}
