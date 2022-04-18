use std::{collections::BTreeSet, sync::Arc};

mod span;
pub use span::Span;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Module {
    pub fns: Vec<FnId>,
    pub exports: Vec<IdentId>,
}
impl Module {
    pub fn empty() -> Self {
        Module { fns: Vec::new(), exports: Vec::new() }
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum Type {
    THole,
    Ti32,
    // Should make this more wasm-y eventually
    TFun(TypeId, TypeId),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct IdentData(String);

impl IdentData {
    pub fn new<S: ToString>(s: S) -> Self {
        Self(s.to_string())
    }
}
impl ToString for IdentData {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug, PartialOrd, Ord)]
pub struct IdentId(salsa::InternId);
crate::impl_intern_key!(IdentId);

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct FnDefn {
    pub name: IdentId,
    pub params: Vec<(IdentId, TypeId)>,
    pub ret_ty: TypeId,
    pub body: ExprId,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Expr {
    Error,
    Li32(i32),
    LStr(String),
    Var(IdentId),
    Sexpr(Vec<ExprId>),
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct ExprId(salsa::InternId);
crate::impl_intern_key!(ExprId);

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct TypeId(salsa::InternId);
crate::impl_intern_key!(TypeId);

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct FnId(salsa::InternId);
crate::impl_intern_key!(FnId);

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct ModId(salsa::InternId);
crate::impl_intern_key!(ModId);

// Is this even a useful abstraction?
#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub struct Node<K, M = ()> {
    pub kind: K,
    pub meta: M,
}

pub type TypeNode<M = ()> = Node<Type, M>;
pub type ExprNode<M = ()> = Node<Expr, M>;
pub type FnDefnNode<M = ()> = Node<FnDefn, M>;

#[salsa::query_group(AstStorage)]
pub trait AstQuery {
    #[salsa::interned]
    fn intern_ident_data(&self, id_data: IdentData) -> IdentId;

    fn main_id(&self) -> IdentId;

    fn ident_is_primitive(&self, id: IdentId) -> bool;
    fn ident_primtive(&self, id: IdentId) -> Option<parity_wasm::elements::Instruction>;

    #[salsa::interned]
    fn intern_expr_node(&self, expr: ExprNode<Span>) -> ExprId;

    fn expr_kind(&self, expr: ExprId) -> Expr;
    fn expr_span(&self, expr: ExprId) -> Span;

    fn expr_bindings(&self, bound: Arc<BTreeSet<IdentId>>, expr: ExprId) -> Bindings;

    #[salsa::interned]
    fn intern_type_node(&self, typ: TypeNode<Span>) -> TypeId;

    #[salsa::interned]
    fn intern_fn_defn(&self, defn: FnDefnNode<Span>) -> FnId;

    fn fn_body(&self, fn_id: FnId) -> ExprId;
    fn fn_params(&self, fn_id: FnId) -> Vec<(IdentId, TypeId)>;
    fn fn_ret(&self, fn_id: FnId) -> TypeId;
    fn fn_name(&self, fn_id: FnId) -> IdentId;


    fn fn_local_vars(&self, fn_id: FnId) -> Vec<IdentId>;

    #[salsa::interned]
    fn intern_module(&self, module: Module) -> ModId;

    fn mod_fns(&self, mod_id: ModId) -> Vec<FnId>;
}

pub fn main_id(db: &dyn AstQuery) -> IdentId {
    db.intern_ident_data(IdentData::new("main"))
}

/// Returns true if this identifier is a primitive and builtin to the language
/// returns false otherwise.
pub fn ident_is_primitive(db: &dyn AstQuery, id: IdentId) -> bool {
    db.ident_primtive(id).is_some()
}

pub fn ident_primtive(db: &dyn AstQuery, id: IdentId) -> Option<parity_wasm::elements::Instruction> {
    use parity_wasm::elements::Instruction;
    match db.lookup_intern_ident_data(id).0.as_str() {
        "+" => Some(Instruction::I32Add),
        "-" => Some(Instruction::I32Sub),
        "*"  => Some(Instruction::I32Mul),
        "/" => Some(Instruction::I32DivS),
        "and" => Some(Instruction::I32And),
        "or" => Some(Instruction::I32Or),
        "xor" => Some(Instruction::I32Xor),
        "shl" => Some(Instruction::I32Shl),
        "shr" => Some(Instruction::I32ShrS),
        "rotl" => Some(Instruction::I32Rotl),
        "rotr" => Some(Instruction::I32Rotr),
        "clz" => Some(Instruction::I32Clz),
        "ctz" => Some(Instruction::I32Ctz),
        "popcnt" => Some(Instruction::I32Popcnt),
        _ => None,
    }
}

pub fn expr_kind(db: &dyn AstQuery, id: ExprId) -> Expr {
    db.lookup_intern_expr_node(id).kind
}

pub fn expr_span(db: &dyn AstQuery, id: ExprId) -> Span {
    db.lookup_intern_expr_node(id).meta
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Bindings {
    free: Arc<BTreeSet<IdentId>>,
    bound: Arc<BTreeSet<IdentId>>,
}
pub fn expr_bindings(db: &dyn AstQuery, bound: Arc<BTreeSet<IdentId>>, id: ExprId) -> Bindings {
    let mut bound: BTreeSet<IdentId> = bound.iter().cloned().collect();
    let mut free: BTreeSet<IdentId> = BTreeSet::new();
    let mut exprs: Vec<ExprId> = vec![id];
    while let Some(expr) = exprs.pop() {
        match db.expr_kind(expr) {
            Expr::Error => panic!("Error expr"),
            Expr::Var(id) => if !bound.contains(&id) {
                free.insert(id);
            },
            Expr::Sexpr(children) => {
                exprs.extend(children);
            },
            _ => {/* These expressions don't affect bindings */},
        }
    }
    Bindings {
        bound: Arc::new(bound),
        free: Arc::new(free),
    }
}


pub fn fn_body(db: &dyn AstQuery, fn_id: FnId) -> ExprId {
    db.lookup_intern_fn_defn(fn_id).kind.body
}
pub fn fn_params(db: &dyn AstQuery, fn_id: FnId) -> Vec<(IdentId, TypeId)> {
    db.lookup_intern_fn_defn(fn_id).kind.params
}
pub fn fn_ret(db: &dyn AstQuery, fn_id: FnId) -> TypeId {
    db.lookup_intern_fn_defn(fn_id).kind.ret_ty
}
pub fn fn_name(db: &dyn AstQuery, fn_id: FnId) -> IdentId {
    db.lookup_intern_fn_defn(fn_id).kind.name
}

pub fn fn_local_vars(db: &dyn AstQuery, fn_id: FnId) -> Vec<IdentId> {
    let params: BTreeSet<IdentId> =
        db.fn_params(fn_id).into_iter().map(|(id, _)| id).collect();

    let bindings = db.expr_bindings(Arc::new(params), db.fn_body(fn_id));
    // In wasm params are considered locals
    // The order here matters because it decides there index later on and params are automatically at the start of locals
    bindings.bound.iter().cloned().collect()
}

pub fn mod_fns(db: &dyn AstQuery, mod_id: ModId) -> Vec<FnId> {
    db.lookup_intern_module(mod_id).fns
}