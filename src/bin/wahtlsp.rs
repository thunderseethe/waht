
use tokio::sync::Mutex;
use tower_lsp::jsonrpc;
use tower_lsp::lsp_types;
use tower_lsp::lsp_types::ReferenceParams;
use tower_lsp::{Client, LanguageServer, LspService, Server};

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

