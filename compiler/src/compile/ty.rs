use core::fmt;

use wasm_encoder::{
    AbstractHeapType, FieldType, HeapType, RefType, StorageType, TypeSection, ValType,
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Ty {
    Unit,
    I32,
    I64,
    F64,
    Bool,
    Range,
    String,
    Pointer(Box<Ty>),
}

impl Ty {
    pub fn val_type(&self) -> Option<ValType> {
        match self {
            Ty::Unit => None,
            Ty::I32 => Some(ValType::I32),
            Ty::I64 => Some(ValType::I64),
            Ty::F64 => Some(ValType::F64),
            Ty::Bool => Some(ValType::I32),
            Ty::Pointer(inner) => Some(ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(inner.struct_type_idx()),
            })),
            Ty::Range => Some(ValType::Ref(RefType {
                nullable: false,
                heap_type: HeapType::Concrete(Ty::Range.struct_type_idx()),
            })),
            Ty::String => Some(ValType::Ref(RefType {
                nullable: false,
                heap_type: HeapType::Concrete(Ty::String.struct_type_idx()),
            })),
        }
    }

    pub fn install(types: &mut TypeSection) {
        types.struct_([]);
        types.struct_([FieldType {
            element_type: StorageType::Val(ValType::I32),
            mutable: true,
        }]);
        types.struct_([FieldType {
            element_type: StorageType::Val(ValType::I64),
            mutable: true,
        }]);
        types.struct_([FieldType {
            element_type: StorageType::Val(ValType::F64),
            mutable: true,
        }]);
        types.struct_([
            FieldType {
                element_type: StorageType::Val(ValType::I32),
                mutable: true,
            },
            FieldType {
                element_type: StorageType::Val(ValType::I32),
                mutable: true,
            },
        ]);
        types.array(&StorageType::I8, false);
        types.struct_([FieldType {
            element_type: StorageType::Val(ValType::Ref(RefType {
                heap_type: HeapType::Abstract {
                    ty: AbstractHeapType::Any,
                    shared: false,
                },
                nullable: false,
            })),
            mutable: true,
        }]);
    }

    pub fn struct_type_idx(&self) -> u32 {
        match self {
            Ty::Unit => 0,
            Ty::I32 => 1,
            Ty::I64 => 2,
            Ty::F64 => 3,
            Ty::Bool => Ty::I32.struct_type_idx(),
            Ty::Range => 4,
            Ty::String => 5,
            Ty::Pointer(_) => 6,
        }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Unit => write!(f, "()"),
            Ty::I32 => write!(f, "i32"),
            Ty::I64 => write!(f, "i64"),
            Ty::F64 => write!(f, "f64"),
            Ty::Bool => write!(f, "bool"),
            Ty::Range => write!(f, "range"),
            Ty::String => write!(f, "string"),
            Ty::Pointer(inner) => write!(f, "&{inner}"),
        }
    }
}
