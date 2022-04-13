
struct Module {
    fns: HashMap<Ident, FnDefn>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Type {
    THole,
    Ti32,
    // Should make this more wasm-y eventually
    TFun(Box<Type>, Box<Type>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Ident(String);

impl Ident {
    pub fn new<S: ToString>(s: S) -> Self {
        Ident(s.to_string())
    }
}
impl From<&str> for Ident {
    fn from(s: &str) -> Self {
        Ident(s.to_string())
    }
}
impl From<String> for Ident {
    fn from(s: String) -> Ident {
        Ident(s)
    }
}

#[derive(PartialEq, Debug)]
pub struct FnDefn {
    name: Ident,
    args: Vec<(Ident, Type)>,
    body: Expr,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Error,
    Li32(i32),
    LStr(String),
    Var(Ident),
    Sexpr(Vec<Expr>),
}

#[macro_export]
macro_rules! sexpr {
    ($($e:expr),*) => {
        Expr::Sexpr(vec![$($e),*])
    }
}

#[macro_export]
macro_rules! var {
    ($id:expr) => {
        crate::ast::Expr::Var(crate::ast::Ident::new($id))
    };
}
