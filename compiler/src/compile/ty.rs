use wasm_encoder::ValType;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Ty {
    Unit,
    I32,
    I64,
    F64,
    Bool,
    Range,
}

impl Ty {
    pub fn val_type(&self) -> Option<ValType> {
        match self {
            Ty::I32 => Some(ValType::I32),
            Ty::I64 => Some(ValType::I64),
            Ty::F64 => Some(ValType::F64),
            Ty::Bool => Some(ValType::I32),
            _ => None,
        }
    }

    pub fn size(&self) -> i32 {
        match self {
            Ty::Unit => 0,
            Ty::I32 => 1,
            Ty::I64 => 2,
            Ty::F64 => 2,
            Ty::Bool => 1,
            Ty::Range => 2,
        }
    }
}
