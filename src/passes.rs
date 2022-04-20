use crate::ast;

#[salsa::query_group]
trait Pass {
    
    fn convert_sexpr_to_prims(&self, expr_id: ast::ExprId) -> {

    }
}