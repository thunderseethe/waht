#![feature(trait_upcasting)]
use std::sync::Arc;


mod ast;
mod lexer;
mod parser;
pub use parser::ParserQuery;
mod codegen;
pub use codegen::Codegen;
mod type_check;
pub use type_check::TypeCheck;

pub fn build_report<T: std::hash::Hash + std::fmt::Display + Eq, S>(key: &S::SourceId, e: Vec<chumsky::error::Simple<T, S>>) -> ariadne::Report<S>
where
    T: std::hash::Hash + std::fmt::Display + Eq,
    S: ariadne::Span + Clone,
{
    use chumsky::error::SimpleReason;

    let start_span = e.iter().map(|e| e.span().start()).min().unwrap_or(0);
    let builder = ariadne::Report::build(ariadne::ReportKind::Error, key.to_owned(), start_span);
    e.into_iter().fold(builder, |mut builder, e| {
        let i = e.expected()
            .filter_map(|s| s.as_ref().map(|s| format!("'{}', ", s)))
            .collect::<String>();
        if let Some(lbl) = e.label() {
            builder = builder.with_message(lbl.to_owned());
        }
        match e.reason() {
            SimpleReason::Unexpected => {
                builder.with_label(ariadne::Label::new(e.span())
                    .with_message(format!("Unexpected input, expected {{ {} }}", i)))
            },
            SimpleReason::Unclosed { span: _span, delimiter } => {
                builder.with_label(ariadne::Label::new(e.span()).with_message(format!("Unclosed delimiter {}", delimiter)))
            }
            SimpleReason::Custom(s) => {
                builder.with_label(ariadne::Label::new(e.span()).with_message(s))
            },
        }
    }).finish()
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceData {
    path: std::path::PathBuf,
    content: std::string::String,
}
impl SourceData {

    pub fn new<P: Into<std::path::PathBuf>, S: ToString>(path: P, content: S) -> Self {
        Self {
            path: path.into(),
            content: content.to_string(),
        }
    }
}
impl TryFrom<&std::path::Path> for SourceData 
{
    type Error = std::io::Error;

    fn try_from(value: &std::path::Path) -> Result<Self, Self::Error> {
        std::fs::read_to_string(value)
            .map(|content| SourceData { 
                path: value.to_path_buf(),
                content
            })
    }
}

#[macro_export]
macro_rules! impl_intern_key {
    ($ty:ident) => {
    impl salsa::InternKey for $ty {
        fn from_intern_id(id: salsa::InternId) -> Self {
            Self(id)
        }

        fn as_intern_id(&self) -> salsa::InternId {
            self.0
        }
    }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub struct SourceId(salsa::InternId);
impl_intern_key!(SourceId);


#[salsa::query_group(SourcesStorage)]
pub trait SourceQuery {
    #[salsa::interned]
    fn intern_source_data(&self, data: SourceData) -> SourceId;

    fn source_length(&self, key: SourceId) -> usize;
    fn source_filename(&self, key: SourceId) -> Arc<String>;

    fn ariadne_source(&self, key: SourceId) -> Arc<ariadne::Source>;
}

fn source_length(db: &dyn SourceQuery, key: SourceId) -> usize {
    db.lookup_intern_source_data(key).content.len()
}

fn source_filename(db: &dyn SourceQuery, key: SourceId) -> Arc<String> {
    let path = db.lookup_intern_source_data(key).path;
    Arc::new(path.file_name()
        .and_then(|os_str| os_str.to_str())
        .map(|s| s.to_owned())
        .unwrap_or_else(|| format!("{}", path.display())))
}

fn ariadne_source(db: &dyn SourceQuery, key: SourceId) -> Arc<ariadne::Source> {
    let source = db.lookup_intern_source_data(key);
    Arc::new(ariadne::Source::from(source.content.as_str()))
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Error {
    TypeCheck(Vec<type_check::TypeError>)
}

#[salsa::database(
    SourcesStorage,
    lexer::LexerStorage,
    ast::AstStorage,
    parser::ParserStorage,
    codegen::CodegenStorage,
    type_check::TypeCheckStorage,
)]
#[derive(Default)]
pub struct Database {
    storage: salsa::Storage<Self>,
}
impl salsa::Database for Database {}
