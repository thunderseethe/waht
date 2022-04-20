use std::sync::Arc;
use std::path::PathBuf;

use tokio::sync::Mutex;
use waht::{Database, SourceData, SourceQuery, Codegen};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let db = Arc::new(Mutex::new(Database::default()));

    let path = PathBuf::from(std::env::args().nth(1).unwrap());
    let content = std::fs::read_to_string(&path)?;
    let wasm_out = path.with_extension("wasm");
    let wat_out = path.with_extension("wat");

    let db = db.lock().await;
    let src_id = db.intern_source_data(SourceData::new(path, content));

    let module= db.codegen(src_id).unwrap();

    let mod_bin = parity_wasm::serialize(module)?;

    std::fs::write(wasm_out, &mod_bin)?;

    let wabt_buf = wabt::Wasm2Wat::new() 
        .read_debug_names(true)
        .convert(&mod_bin)?;

    std::fs::write(wat_out, String::from_utf8(wabt_buf.as_ref().to_vec())?)?;

    Ok(())
}