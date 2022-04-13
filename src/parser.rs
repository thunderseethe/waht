use chumsky::prelude::*;
use crate::ast::{Expr, Ident, Type, FnDefn};
use crate::lexer::{Token, lexer};

trait Lispy<O, E: chumsky::Error<Token>> 
where
    Self: Sized,
{

    fn parens(self) -> chumsky::combinator::DelimitedBy<Self, BoxedParser<'static, Token, Token, E>, BoxedParser<'static, Token, Token, E>, Token, Token>;
}

impl<T, O, E> Lispy<O, E> for T 
where
    T: Parser<Token, O, Error=E>,
    E: chumsky::Error<Token> + 'static,
{
    fn parens(self) ->  chumsky::combinator::DelimitedBy<Self, BoxedParser<'static, Token, Token, E>, BoxedParser<'static, Token, Token, E>, Token, Token> {
        self.delimited_by(just(Token::LParen).boxed(), just(Token::RParen).boxed())
    }
}

pub type Span = std::ops::Range<usize>;

pub type Spanned<T> = (T, Span);

fn expr_parser() -> impl Parser<Token, Expr, Error=Simple<Token>> {
    recursive(|expr: Recursive<Token, Expr, Simple<Token>>| {
        let val = select! {
            Token::NumLit(num) => Expr::Li32(num as i32),
            Token::StrLit(str) => Expr::LStr(str),
            Token::Ident(id) => Expr::Var(id.into())
        }.labelled("value");

        let sexpr = expr.repeated()
            .at_least(1)
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .labelled("s-expr")
            .collect::<Vec<_>>()
            .map(Expr::Sexpr);

        val.or(sexpr)
           .recover_with(nested_delimiters(Token::LParen, Token::RParen, [], |span| Expr::Error))
    })
    .boxed()
}

fn type_parser() -> impl Parser<Token, Type, Error=Simple<Token>> {
    let atom_ty = filter_map(|span, t| match t {
        Token::Ident(s) if s == "i32" => Ok(Type::Ti32),
        _ => Err(Simple::custom(span, "expected type to be one of { i32 }")),
    });

    let typ = just(Token::FnArrow)
        .ignore_then(atom_ty.repeated().at_least(2).map(|tys| {
            tys.into_iter().rev().reduce(|a, b| Type::TFun(Box::new(a), Box::new(b))).unwrap()
        }))
        .parens();

    typ.or(atom_ty).boxed()
}


fn fn_parser() -> impl Parser<Token, FnDefn, Error=Simple<Token>> {
    let ident = select! {
        Token::Ident(l) => l.into()
    };
    let arg_list = ident
                      .then(type_parser())
                      .repeated()
                      .parens();

    just(Token::FunctionKw)
        .ignore_then(ident)
        .then(arg_list)
        .then(expr_parser())
        .map_with_span(|((name, args), body), _span| FnDefn { name, args, body })
        .boxed()
}

#[derive(PartialEq, Debug)]
pub enum ParseDefn {
    Function(FnDefn),
}

#[derive(PartialEq, Debug)]
pub struct ParseModule(Vec<Defn>);

fn module_parser() -> impl Parser<Token, ParseModule, Simple<Token>> {
    let fn_defn = fn_parser().map(ParseDefn::Function);

    let defn = fn_defn.boxed();

    defn.repeated()
        .map(ParseModule)
}

fn parse_with<T, E>(p: impl Parser<Token, T, Error=E>, input: &str) -> Result<T, Vec<E>> 
where
    E: chumsky::Error<Token, Span=Span>,
{
    let end = input.len();
    let tokens = lexer().parse(input).map_err(|_| vec![])?;

    let stream: chumsky::Stream<'_, Token, Span, std::vec::IntoIter<(Token, Span)>> = chumsky::Stream::from_iter(end..end + 1, tokens.into_iter());

    p.parse(stream)
}

fn parse(input: &str) -> Result<ParseModule, Vec<Simple<Token>>> {
    parse_with(module_parser(), input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Expr::*;
    use crate::{var, sexpr};
    
    #[test]
    fn parse_lit_i32() {
        assert_eq!(parse_with(expr_parser(), "3"), Ok(Expr::Li32(3)))
    }

    #[test]
    fn parse_lit_str() {
        assert_eq!(parse_with(expr_parser(), "\"a str lit\""), Ok(Expr::LStr("a str lit".to_string())))
    }

    #[test]
    fn parse_var() {
        assert_eq!(parse_with(expr_parser(), "a_var"), Ok(var!("a_var")))
    }

    #[test]
    fn parse_control_as_var() {
        assert_eq!(parse_with(expr_parser(), "+"), Ok(var!("+")));
        assert_eq!(parse_with(expr_parser(), "<$>"), Ok(var!("<$>")))
    }

    #[test]
    fn parse_sexpr() {
        assert_eq!(parse_with(expr_parser(), "(+ 1 2 3)"), Ok(sexpr![var!("+"), Li32(1), Li32(2), Li32(3)]))
    }

    #[test]
    fn parse_fn() {
        assert_eq!(parse_with(fn_parser(), "(fn id ((x i32)) x)"), Ok(Defn::Function { 
            name: Ident::new("id"),
            args: vec![(Ident::new("x"), Type::Ti32)],
            body: var!("x"),
        }))
    }
}
