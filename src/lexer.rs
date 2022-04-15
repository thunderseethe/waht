use std::sync::Arc;

use crate::ast::Span;
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
    FnKw,
}

use std::fmt;
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Token::*;
        match self {
            NumLit(i) => write!(f, "{}", i),
            StrLit(s) => write!(f, "\"{}\"", s),
            Ident(id) => write!(f, "{}", id),
            LParen => write!(f, "("),
            RParen => write!(f, ")"),
            FnArrow => write!(f, "->"),
            FnKw => write!(f, "fn"),
        }
    }
}

pub fn lexer(key: SourceId) -> impl Parser<char, Vec<(Token, Span)>, Error=Simple<char>> {
    let num = text::int(10)
        .map(|istr: String| Token::NumLit(istr.parse().unwrap()));

    let string = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(|str| Token::StrLit(str))
        .boxed();

    let lparen = just('(').map(|_| Token::LParen);
    let rparen = just(')').map(|_| Token::RParen);
    // Todo we can allow unicode characters for this
    let non_alpha = filter(|c: &char| c.is_ascii_punctuation() && *c != '#')
        .repeated()
        .at_least(1)
        .collect::<String>() 
        .map(|s| match s.as_str() {
            "->" => Token::FnArrow,
            _ => Token::Ident(s),
        })
        .boxed();

    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "fn" => Token::FnKw,
        _ => Token::Ident(ident),
    });
    
    let token = choice((
        num,
        lparen,
        rparen,
        string,
        non_alpha,
        ident
    ))
    .boxed()
    .recover_with(skip_then_retry_until([]));

    let comment = just("#").then(take_until(just('\n'))).padded();

    token
        .map_with_span(move |tok, span: std::ops::Range<usize>| (tok, Span::new(key, span)))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
        .boxed()
}

use crate::SourceId;

#[salsa::query_group(LexerStorage)]
pub trait Lexer: crate::SourceQuery { 
    fn lex(&self, key: SourceId) -> Arc<Vec<(Token, Span)>>;
}

pub fn lex(db: &dyn Lexer, key: SourceId) -> Arc<Vec<(Token, Span)>> {
    let s = db.lookup_intern_source_data(key);
    match lexer(key).parse(s.content.as_str()) {
        Ok(tokens) => Arc::new(tokens),
        // Since salsa error reporting is in an unfinished state, print our error and panic for now
        Err(e) => {
            let report = crate::build_report(&(), e);
            let mut source = db.ariadne_source(key);
            report.eprint(Arc::make_mut(&mut source)).unwrap();
            panic!();
        },
    }
}
