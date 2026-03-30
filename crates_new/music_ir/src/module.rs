use crate::{IrDataLayouts, IrExprTy};

#[derive(Debug, Clone)]
pub struct IrModuleInfo {
    pub expr_tys: Box<[IrExprTy]>,
    pub data_layouts: IrDataLayouts,
}
