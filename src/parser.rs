use crate::lexer::Token;
use crate::{ast::*, SourceId, SourceQuery};
use chumsky::prelude::*;
use std::sync::Arc;

trait Lispy<O, E: chumsky::Error<Token>>
where
    Self: Sized,
{
    fn parens(
        self,
    ) -> chumsky::combinator::DelimitedBy<
        Self,
        BoxedParser<'static, Token, Token, E>,
        BoxedParser<'static, Token, Token, E>,
        Token,
        Token,
    >;
}

impl<T, O, E> Lispy<O, E> for T
where
    T: Parser<Token, O, Error = E>,
    E: chumsky::Error<Token> + 'static,
{
    fn parens(
        self,
    ) -> chumsky::combinator::DelimitedBy<
        Self,
        BoxedParser<'static, Token, Token, E>,
        BoxedParser<'static, Token, Token, E>,
        Token,
        Token,
    > {
        self.delimited_by(just(Token::LParen).boxed(), just(Token::RParen).boxed())
    }
}

fn expr_parser<'a>(
    db: &'a dyn AstQuery,
) -> impl Parser<Token, ExprId, Error = Simple<Token, Span>> + 'a {
    recursive(|expr: Recursive<Token, ExprId, Simple<Token, Span>>| {
        let val = select! {
            Token::NumLit(num) => Expr::Li32(num as i32),
            Token::StrLit(str) => Expr::LStr(str),
            Token::Ident(id) => {
                let id = db.intern_ident_data(IdentData::new(id));
                Expr::Var(id)
            }
        }
        .labelled("value");

        let sexpr = expr
            .repeated()
            .at_least(1)
            .parens()
            .labelled("s-expr")
            .map(Expr::Sexpr)
            .boxed();

        val.or(sexpr)
            .map_with_span(|kind, meta| db.intern_expr_node(Node { kind, meta }))
    })
    .boxed()
}

fn optional<I, O, P: chumsky::Parser<I, O>>(p: P) -> impl Parser<I, Option<O>, Error=P::Error> 
where
    I: Clone,
    O: Clone,
{
    choice((
        p.map(Some),
        empty().to(None)
    ))
}

fn type_parser<'a>(
    db: &'a dyn AstQuery,
) -> impl Parser<Token, TypeId, Error = Simple<Token, Span>> + 'a {
    let atom_ty = filter_map(|span: Span, t| match t {
        Token::Ident(s) if s == "i32" => Ok((Type::Ti32, span)),
        _ => Err(Simple::custom(span, "Expected type to be one of { i32 }")),
    })
    .labelled("atomic type");

    let typ = just(Token::FnArrow)
        .ignore_then(atom_ty)
        .then(atom_ty.repeated().at_least(1))
        .map(|(a, b)| (b, a))
        .foldr(|(param, param_span), (ret, ret_span)| {
            let param_id = db.intern_type_node(Node {
                kind: param,
                meta: param_span.into(),
            });
            let ret_id = db.intern_type_node(Node {
                kind: ret,
                meta: ret_span.into(),
            });
            (Type::TFun(param_id, ret_id), param_span + ret_span)
        })
        .parens()
        .labelled("function type")
        .recover_with(nested_delimiters(
            Token::LParen,
            Token::RParen,
            [],
            |span| (Type::THole, span),
        ))
        .boxed();

    typ.or(atom_ty)
        .map(|(kind, meta)| db.intern_type_node(Node { kind, meta }))
        .boxed()
}

fn fn_parser<'a>(
    db: &'a dyn AstQuery,
) -> impl Parser<Token, FnId, Error = Simple<Token, Span>> + 'a {
    let ident = select! {
        Token::Ident(l) => db.intern_ident_data(IdentData::new(l))
    };
    let fn_sig = ident
        .then(type_parser(db))
        .parens()
        .repeated()
        .then(type_parser(db))
        .parens()
        .labelled("arg list")
        .boxed();

        choice((
            just(Token::ExportKw).to(true),
            empty().to(false)
        )) 
        .then_ignore(just(Token::FnKw))
        .then(ident)
        .then(fn_sig)
        .then(expr_parser(db))
        .parens()
        .map_with_span(|(((export, name), (params, ret_ty)), body), meta| {
            db.intern_fn_defn(Node {
                kind: FnDefn { name, params, ret_ty, body, export },
                meta,
            })
        })
        .labelled("function")
        .boxed()
}

#[derive(PartialEq, Debug)]
pub enum ParseDefn {
    Function(FnId),
}

fn module_parser<'a>(
    db: &'a dyn AstQuery,
) -> impl Parser<Token, Module, Error = Simple<Token, Span>> + 'a {
    let fn_defn = fn_parser(db).map(ParseDefn::Function);

    let defn = fn_defn.boxed();

    defn.repeated().at_least(1).map(|defns| {
        defns.into_iter().fold(Module::empty(), |mut module, defn| {
            match defn {
                ParseDefn::Function(id) => module.fns.push(id),
            };
            module
        })
    })
}

#[salsa::query_group(ParserStorage)]
pub trait ParserQuery: crate::ast::AstQuery + crate::lexer::Lexer {
    fn parse(&self, key: crate::SourceId) -> crate::ast::ModId;
}

fn parse(db: &dyn ParserQuery, key: crate::SourceId) -> crate::ast::ModId {
    let tokens = db.lex(key);
    let end = db.source_length(key);
    let mod_res = module_parser(db).parse(chumsky::Stream::from_iter(
        Span::new(key, end..end + 1),
        tokens.iter().cloned(),
    ));
    match mod_res {
        Ok(m) => {
            db.intern_module(m)
        },
        Err(e) => {
            let report = crate::build_report(&key, e);
            let cache = DbCache(db, None);
            report.eprint(cache).unwrap();
            panic!()
        }
    }
}

// This is a terrible hack
struct DbCache<'a>(&'a dyn SourceQuery, Option<Arc<ariadne::Source>>);
impl<'d> ariadne::Cache<SourceId> for DbCache<'d> {
    fn fetch(&mut self, id: &SourceId) -> Result<&ariadne::Source, Box<dyn std::fmt::Debug + '_>> {
        self.1 = Some(self.0.ariadne_source(*id));
        Ok(self.1.as_ref().unwrap().as_ref())
    }

    fn display<'a>(&self, id: &'a SourceId) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new(format!(
            "{}",
            self.0.lookup_intern_source_data(*id).path.display()
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;
    use crate::{ast::Expr::*, SourceQuery, SourceData};

    #[test]
    fn parse_lit_i32() {
        let db = crate::Database::default();
        let src_id = db.intern_source_data(SourceData::new("/interactive", "(fn three (i32) 3)"));
        
        let mod_id = db.parse(src_id);
        let mods = db.lookup_intern_module(mod_id);
        let fun = db.lookup_intern_fn_defn(mods.fns[0]);
        let body = db.lookup_intern_expr_node(fun.kind.body);
        assert_eq!(body.kind, Li32(3));
    }

    #[test]
    fn parse_lit_str() {
        let db = crate::Database::default();
        let src_id = db.intern_source_data(SourceData::new("/interactive", 
            "(fn a (i32) \"a str lit\")"));
        let mod_id = db.parse(src_id);
        let module = db.lookup_intern_module(mod_id);
        assert_eq!(
            db.lookup_intern_expr_node(db.lookup_intern_fn_defn(module.fns[0]).kind.body).kind,
            LStr("a str lit".to_owned()));
    }

    #[test]
    fn parse_var() {
        let db = crate::Database::default();
        let src_id = db.intern_source_data(SourceData { 
            path: PathBuf::from("/interactive"),
            content: "(fn name (i32) a_var)".to_owned() 
        });
        let mod_id = db.parse(src_id);
        let fun = db.lookup_intern_fn_defn(db.lookup_intern_module(mod_id).fns[0]);
        assert_eq!(db.lookup_intern_ident_data(fun.kind.name).to_string(), "name");
        assert_eq!(fun.kind.params, vec![]);
        assert!(matches!(db.lookup_intern_expr_node(fun.kind.body).kind, Var(i) if db.lookup_intern_ident_data(i) == "a_var".into()));
    }

    #[test]
    fn parse_control_as_var() {
        let db = crate::Database::default();
        let source_id = db.intern_source_data(SourceData::new(
            "/interactive",
            concat!(
                "(fn + ((x i32) (y i32) i32) (+ x y))",
                "(fn <$> ((f (-> i32 i32)) (x i32) i32) (f x))"
            ),
        ));
        let mod_id = db.parse(source_id);
        let module = db.lookup_intern_module(mod_id);

        let plus_expr = db.lookup_intern_fn_defn(module.fns[0]);
        assert_eq!(db.lookup_intern_ident_data(plus_expr.kind.name), IdentData::new("+"));

        let fmap_expr = db.lookup_intern_fn_defn(module.fns[1]);
        assert_eq!(db.lookup_intern_ident_data(fmap_expr.kind.name), IdentData::new("<$>"));
    }

    /*#[test]
    fn parse_sexpr() {
        let db = crate::Database::default();
        let src_id = db.intern_source_data(SourceData::new(
            "/interactive",
            "(fn sum (i32) (+ 1 2 3))",
        ));
  
        let mod_id = db.parse(src_id);
        let mods = db.lookup_intern_module(mod_id);
        let fun = db.lookup_intern_fn_defn(mods.fns[0]);
        let body = db.lookup_intern_expr_node(fun.kind.body);
        let sexprs = match body.kind {
            Sexpr(exprs) => {
                exprs.into_iter()
                    .map(|e| db.lookup_intern_expr_node(e))
                    .collect::<Vec<_>>()
            },
            _ => {panic!("Expect sexpr body")},
        };
    }*/

    /*
    #[test]
    fn parse_fn() {
        assert_eq!(parse_with(fn_parser(), "(fn id ((x i32)) x)"), Ok(FnDefn {
            name: IdentData::new("id"),
            args: vec![(IdentData::new("x"), Type::Ti32)],
            body: var!("x"),
        }))
    } */
}
