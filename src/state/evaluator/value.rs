use std::{cell::UnsafeCell, collections::BTreeMap, rc::Rc};

use crate::parser::expr::{literal::Number, CommentedExpr};

use super::EvalContext;

pub enum Value {
    Pair(String, Rc<UnsafeCell<Value>>),
    Number(Number),
    String(String),
    Bool(bool),
    // entrys, current call status, eval context, expr
    Function(
        Vec<String>,
        BTreeMap<String, Rc<UnsafeCell<Value>>>,
        Rc<UnsafeCell<EvalContext>>,
        CommentedExpr,
    ),
    NativeFunction(
        String,
        Vec<String>,
        BTreeMap<String, Rc<UnsafeCell<Value>>>,
        unsafe extern "C" fn(usize, va: *mut *mut Value) -> UnsafeCell<Value>,
    ),
    Array(Vec<Rc<UnsafeCell<Value>>>),
    Object(BTreeMap<String, Rc<UnsafeCell<Value>>>),
    Enum(BTreeMap<String, Value>),
    /// could not return a null without specify the type
    Null,
    /// same as null
    Error(String),
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Self::Pair(arg0, arg1) => Self::Pair(
                arg0.clone(),
                Rc::new(UnsafeCell::new(unsafe { &*arg1.get() }.clone())),
            ),
            Self::Number(arg0) => Self::Number(arg0.clone()),
            Self::String(arg0) => Self::String(arg0.clone()),
            Self::Bool(arg0) => Self::Bool(arg0.clone()),
            Self::Function(arg0, arg1, arg2, arg3) => {
                Self::Function(arg0.clone(), arg1.clone(), arg2.clone(), arg3.clone())
            }
            Self::NativeFunction(arg0, arg1, arg2, arg3) => {
                Self::NativeFunction(arg0.clone(), arg1.clone(), arg2.clone(), arg3.clone())
            }
            Self::Array(arg0) => Self::Array(
                arg0.iter()
                    .map(|arg1| Rc::new(UnsafeCell::new(unsafe { &*arg1.get() }.clone())))
                    .collect(),
            ),
            Self::Object(arg0) => Self::Object(
                arg0.iter()
                    .map(|(arg0, arg1)| {
                        (
                            arg0.clone(),
                            Rc::new(UnsafeCell::new(unsafe { &*arg1.get() }.clone())),
                        )
                    })
                    .collect(),
            ),
            Self::Enum(arg0) => Self::Enum(arg0.clone()),
            Self::Null => Self::Null,
            Self::Error(arg0) => Self::Error(arg0.clone()),
        }
    }
}
impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Pair(s, o) => f
                .debug_tuple("Pair")
                .field(s)
                .field(unsafe { &*o.as_ref().get() })
                .finish(),
            Self::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::Function(arg0, arg1, _arg2, _arg3) => {
                f.debug_tuple("Function").field(arg0).field(arg1).finish()
            }
            Self::NativeFunction(arg0, arg1, arg2, arg3) => f
                .debug_tuple("NativeFunction")
                .field(arg0)
                .field(arg1)
                .field(arg2)
                .field(arg3)
                .finish(),
            Self::Array(arg0) => f
                .debug_tuple("Array")
                .field(
                    &arg0
                        .iter()
                        .map(|t| unsafe { &*t.as_ref().get() })
                        .collect::<Vec<_>>(),
                )
                .finish(),
            Self::Object(arg0) => f
                .debug_tuple("Object")
                .field(
                    &arg0
                        .iter()
                        .map(|(k, v)| (k, unsafe { &*v.as_ref().get() }))
                        .collect::<BTreeMap<_, _>>(),
                )
                .finish(),
            Self::Enum(arg0) => f.debug_tuple("Enum").field(arg0).finish(),
            Self::Null => write!(f, "Null"),
            Self::Error(arg0) => f.debug_tuple("Error").field(arg0).finish(),
        }
    }
}
