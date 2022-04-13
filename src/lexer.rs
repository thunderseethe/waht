use crate::parser::{Span, Spanned};
use chumsky::prelude::*;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Token {
    NumLit(usize), 
    StrLit(String),
    Ident(String),
    // Semantic control characters
    LParen,
    RParen,
    FnArrow,
    // Keywords
    FunctionKw,
}

pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error=Simple<char>> {
    let num = text::int(10)
        .map(|istr: String| Token::NumLit(istr.parse().unwrap()));

    let string = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::StrLit)
        .boxed();

    let lparen = just('(').map(|_| Token::LParen);
    let rparen = just(')').map(|_| Token::RParen);
    // Todo we can allow unicode characters for this
    let non_alpha = filter(|c: &char| c.is_ascii_punctuation() && *c != '#').repeated().at_least(1)
        .collect::<String>() 
        .map(Token::Ident)
        .boxed();

    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "fn" => Token::FunctionKw,
        _ => Token::Ident(ident),
    });
    
    let token = num
        .or(lparen)
        .or(rparen)
        .or(string)
        .or(non_alpha)
        .or(ident)
        .boxed()
        .recover_with(skip_then_retry_until([]));

    let comment = just("#").then(take_until(just('\n'))).padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
        .boxed()
}
