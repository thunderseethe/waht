use crate::SourceId;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Span {
    source_id: SourceId,
    start: usize,
    end: usize,
}
impl std::ops::Add for Span {
    type Output = Span;

    fn add(self, rhs: Span) -> Span {
        assert_eq!(self.source_id, rhs.source_id);
        Self {
            start: self.start.min(rhs.start),
            end: self.end.max(rhs.end),
            ..self
        }
    }
}
impl chumsky::Span for Span {
    type Context = SourceId;
    type Offset = usize;

    fn new(context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Self { 
            source_id: context, 
            start: range.start, 
            end: range.end
        }
    }

    fn context(&self) -> Self::Context {
        self.source_id
    }

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    } 
}
impl ariadne::Span for Span {
    type SourceId = SourceId;

    fn source(&self) -> &Self::SourceId {
        &self.source_id
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    pub fns: Vec<FnId>,
}
impl Module {
    pub fn empty() -> Self {
        Module { fns: Vec::new() }
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

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct IdentId(salsa::InternId);
crate::impl_intern_key!(IdentId);

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct FnDefn {
    pub name: IdentId,
    pub args: Vec<(IdentId, TypeId)>,
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

// Is this even a useful abstraction?
#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub struct Node<K, M=()> {
    pub kind: K,
    pub meta: M,
}

type TypeNode<M=()> = Node<Type, M>;
type ExprNode<M=()> = Node<Expr, M>;
type FnDefnNode<M=()> = Node<FnDefn, M>;

#[salsa::query_group(AstStorage)]
pub trait AstQuery {
    #[salsa::interned]
    fn intern_ident_data(&self, id_data: IdentData) -> IdentId;

    #[salsa::interned]
    fn intern_expr_node(&self, expr: ExprNode<Span>) -> ExprId;

    #[salsa::interned]
    fn intern_type_node(&self, typ: TypeNode<Span>) -> TypeId;

    #[salsa::interned]
    fn intern_fn_defn(&self, expr: FnDefnNode<Span>) -> FnId;
}