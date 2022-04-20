use std::{collections::BTreeSet, sync::Arc};

mod span;
pub use span::Span;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Module {
    pub fns: Vec<FnDefnNode<Span>>,
}
impl Module {
    pub fn empty() -> Self {
        Module { fns: Vec::new() }
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum Type {
    THole,
    TUnit,
    Ti32,
    // Should make this more wasm-y eventually
    TFun { params: Vec<TypeId>, ret: TypeId },
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct IdentData(String);

impl IdentData {
    pub fn new<S: ToString>(s: S) -> Self {
        Self(s.to_string())
    }
}
impl From<&str> for IdentData {
    fn from(s: &str) -> Self {
        IdentData::new(s)
    }
}
impl From<String> for IdentData {
    fn from(s: String) -> Self {
        IdentData::new(s)
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
    pub params: Vec<(IdentId, TypeNode<Span>)>,
    pub ret_ty: TypeNode<Span>,
    pub body: ExprNode<Span>,
    pub export: bool,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Expr {
    Error,
    Li32(i32),
    LStr(String),
    Var(IdentId),
    Sexpr(Vec<ExprNode<Span>>),
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

pub type TypeNode<M = ()> = Node<TypeId, M>;
pub type ExprNode<M = ()> = Node<ExprId, M>;
pub type FnDefnNode<M = ()> = Node<FnId, M>;

#[salsa::query_group(AstStorage)]
pub trait AstQuery {
    #[salsa::interned]
    fn intern_ident_data(&self, id_data: IdentData) -> IdentId;

    fn main_id(&self) -> IdentId;

    fn ident_is_primitive(&self, id: IdentId) -> bool;
    fn ident_primtive(&self, id: IdentId) -> Option<parity_wasm::elements::Instruction>;

    #[salsa::interned]
    fn intern_expr_data(&self, expr: Expr) -> ExprId;

    fn expr_bindings(&self, bound: Arc<BTreeSet<IdentId>>, expr: ExprId) -> Bindings;

    #[salsa::interned]
    fn intern_type_data(&self, typ: Type) -> TypeId;

    #[salsa::interned]
    fn intern_fn_data(&self, defn: FnDefn) -> FnId;

    fn fn_body(&self, fn_id: FnId) -> ExprNode<Span>;
    fn fn_params(&self, fn_id: FnId) -> Vec<(IdentId, TypeNode<Span>)>;
    fn fn_ret(&self, fn_id: FnId) -> TypeNode<Span>;
    fn fn_name(&self, fn_id: FnId) -> IdentId;
    fn fn_export(&self, fn_id: FnId) -> bool;


    fn fn_local_vars(&self, fn_id: FnId) -> Vec<IdentId>;

    #[salsa::interned]
    fn intern_module(&self, module: Module) -> ModId;

    fn mod_fns(&self, mod_id: ModId) -> Vec<FnDefnNode<Span>>;
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


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Bindings {
    free: Arc<BTreeSet<IdentId>>,
    bound: Arc<BTreeSet<IdentId>>,
}
pub fn expr_bindings(db: &dyn AstQuery, bound: Arc<BTreeSet<IdentId>>, id: ExprId) -> Bindings {
    let bound: BTreeSet<IdentId> = bound.iter().cloned().collect();
    let mut free: BTreeSet<IdentId> = BTreeSet::new();
    let mut exprs: Vec<ExprId> = vec![id];
    while let Some(expr) = exprs.pop() {
        match db.lookup_intern_expr_data(expr) {
            Expr::Error => panic!("Error expr"),
            Expr::Var(id) => if !bound.contains(&id) {
                free.insert(id);
            },
            Expr::Sexpr(children) => {
                exprs.extend(children.into_iter().map(|n| n.kind));
            },
            _ => {/* These expressions don't affect bindings */},
        }
    }
    Bindings {
        bound: Arc::new(bound),
        free: Arc::new(free),
    }
}


pub fn fn_body(db: &dyn AstQuery, fn_id: FnId) -> ExprNode<Span> {
    db.lookup_intern_fn_data(fn_id).body
}
pub fn fn_params(db: &dyn AstQuery, fn_id: FnId) -> Vec<(IdentId, TypeNode<Span>)> {
    db.lookup_intern_fn_data(fn_id).params
}
pub fn fn_ret(db: &dyn AstQuery, fn_id: FnId) -> TypeNode<Span> {
    db.lookup_intern_fn_data(fn_id).ret_ty
}
pub fn fn_name(db: &dyn AstQuery, fn_id: FnId) -> IdentId {
    db.lookup_intern_fn_data(fn_id).name
}
pub fn fn_export(db: &dyn AstQuery, fn_id: FnId) -> bool {
    db.lookup_intern_fn_data(fn_id).export
}

pub fn fn_local_vars(db: &dyn AstQuery, fn_id: FnId) -> Vec<IdentId> {
    let params: BTreeSet<IdentId> =
        db.fn_params(fn_id).into_iter().map(|(id, _)| id).collect();

    let bindings = db.expr_bindings(Arc::new(params), db.fn_body(fn_id).kind);
    // In wasm params are considered locals
    // The order here matters because it decides there index later on and params are automatically at the start of locals
    bindings.bound.iter().cloned().collect()
}

pub fn mod_fns(db: &dyn AstQuery, mod_id: ModId) -> Vec<FnDefnNode<Span>> {
    db.lookup_intern_module(mod_id).fns
}