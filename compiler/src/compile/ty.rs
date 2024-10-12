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

#[derive(Debug, Clone, Eq, PartialEq, Copy)]
pub enum GcType {
    Struct(u32),
    Array(u32),
}

impl GcType {
    pub fn idx(&self) -> u32 {
        match self {
            GcType::Array(idx) | GcType::Struct(idx) => *idx,
        }
    }
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
                nullable: false,
                heap_type: HeapType::Concrete(inner.gc_type().idx()),
            })),
            Ty::Range => Some(ValType::Ref(RefType {
                nullable: false,
                heap_type: HeapType::Concrete(Ty::Range.gc_type().idx()),
            })),
            Ty::String => Some(ValType::Ref(RefType {
                nullable: false,
                heap_type: HeapType::Concrete(Ty::String.gc_type().idx()),
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

    pub fn gc_type(&self) -> GcType {
        match self {
            Ty::Unit => GcType::Struct(0),
            Ty::I32 => GcType::Struct(1),
            Ty::I64 => GcType::Struct(2),
            Ty::F64 => GcType::Struct(3),
            Ty::Bool => Ty::I32.gc_type(),
            Ty::Range => GcType::Struct(4),
            Ty::String => GcType::Array(5),
            Ty::Pointer(_) => GcType::Struct(6),
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
