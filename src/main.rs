#![feature(trait_upcasting)]
use std::collections::HashMap;
use std::sync::Arc;

use codegen::Codegen;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc;
use tower_lsp::lsp_types;
use tower_lsp::lsp_types::ReferenceParams;
use tower_lsp::{Client, LanguageServer, LspService, Server};

mod ast;
mod lexer;
mod parser;
mod codegen;

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

    fn new<P: Into<std::path::PathBuf>, S: ToString>(path: P, content: S) -> Self {
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

#[salsa::database(
    SourcesStorage,
    lexer::LexerStorage,
    ast::AstStorage,
    parser::ParserStorage,
    codegen::CodegenStorage,
)]
#[derive(Default)]
struct Database {
    storage: salsa::Storage<Self>,
}
impl salsa::Database for Database {}

struct Backend {
    client: Client,
    db: Arc<Mutex<Database>>,
    open_docs: Arc<Mutex<HashMap<std::path::PathBuf, SourceId>>>,
}
impl Backend {
    fn new(client: Client, db: Arc<Mutex<Database>>) -> Self {
        Self {
            client,
            db,
            open_docs: Arc::new(Mutex::new(HashMap::new())),
        }

    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(
        &self,
        _params: lsp_types::InitializeParams,
    ) -> jsonrpc::Result<lsp_types::InitializeResult>
    {
        let mut res = lsp_types::InitializeResult::default();
        res.server_info = Some(lsp_types::ServerInfo {
            name: "waht".to_string(),
            version: None,
        });

        res.capabilities.definition_provider = Some(lsp_types::OneOf::Left(true));

        Ok(res)
    }

    async fn goto_definition(
        &self,
        params: lsp_types::GotoDefinitionParams
    ) -> jsonrpc::Result<Option<lsp_types::GotoDefinitionResponse>> {
        self.client.log_message(lsp_types::MessageType::WARNING, 
                                format!("GoTo Definition URI: {:?}({}:{})", params.text_document_position_params.text_document.uri,
                                                                            params.text_document_position_params.position.line,
                                                                            params.text_document_position_params.position.character)).await;
        Ok(None)
    }

    async fn did_open(
        &self,
        params: lsp_types::DidOpenTextDocumentParams,
    ) -> () {
        let path = std::path::PathBuf::from(params.text_document.uri.path());
        println!("{}", path.as_path().display());
        self.client.log_message(lsp_types::MessageType::WARNING,
            format!("did open {}", path.as_path().display())).await;

        let db = self.db.lock().await;
        let source_id = db.intern_source_data(SourceData { 
            path: path.clone(),
            content: params.text_document.text 
        });

        self.open_docs.lock().await.insert(path, source_id); 
    } 

    async fn references(
        &self,
        _params: ReferenceParams,
    ) -> jsonrpc::Result<Option<Vec<lsp_types::Location>>> {
        Ok(None)
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let db = Arc::new(Mutex::new(Database::default()));
    /*let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend::new(client, db));
    Server::new(stdin, stdout, socket).serve(service).await;*/

    let path = std::path::PathBuf::from(std::env::args().nth(1).unwrap());
    let content = std::fs::read_to_string(&path)?;
    let wasm_out = path.with_extension("wasm");
    let wat_out = path.with_extension("wat");

    let db = db.lock().await;
    let src_id = db.intern_source_data(SourceData { path, content });

    let module= db.codegen(src_id);

    let mod_bin = parity_wasm::serialize(module)?;

    std::fs::write(wasm_out, &mod_bin)?;

    let wabt_buf = wabt::Wasm2Wat::new() 
        .read_debug_names(true)
        .convert(&mod_bin)?;

    std::fs::write(wat_out, String::from_utf8(wabt_buf.as_ref().to_vec())?)?;

    Ok(())
}
