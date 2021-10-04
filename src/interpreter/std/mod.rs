mod numerical;

use alloc::{
    borrow::ToOwned,
    boxed::Box,
    collections::BTreeMap,
    fmt::format,
    format,
    rc::Rc,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use core::cell::UnsafeCell;

use crate::parser::{
    expr::{literal::Number, CommentedExpr, Expr},
    ty::Type,
};

use self::numerical::add_std_numerical;

use super::{
    interpreter::{EvalContext, Value},
    type_checker::TypeCheckContext,
    Runner,
};

struct NativeFunction {
    name: String,
    args: Vec<String>,
    filled_args: BTreeMap<String, Rc<UnsafeCell<Value>>>,
    ptr: extern "C" fn(usize, va: *mut *mut Value) -> UnsafeCell<Value>,
}

impl NativeFunction {
    fn into_value(&self) -> Rc<UnsafeCell<Value>> {
        Rc::new(UnsafeCell::new(Value::NativeFunction(
            self.name.clone(),
            self.args.clone(),
            self.filled_args.clone(),
            self.ptr.clone(),
        )))
    }
}
unsafe extern "C" fn err(args_count: usize, va: *mut *mut Value) -> UnsafeCell<Value> {
    let mut vec = Vec::from_raw_parts(va, args_count, 2);
    let v1 = &mut *vec[0];
    if let Value::String(s) = v1 {
        UnsafeCell::new(Value::Error(s.clone()))
    } else {
        panic!("YOU DOESN'T EVEN KNOW HOW TO USE ERR WITH A SIMPLE STRING OR WHAT?")
    }
}
unsafe extern "C" fn id(args_count: usize, va: *mut *mut Value) -> UnsafeCell<Value> {
    let mut vec = Vec::from_raw_parts(va, args_count, 2);
    let v1 = &mut *vec[0];
    UnsafeCell::new(v1.clone())
}

pub fn add_std(r: &mut Runner) {
    //numerical
    add_std_numerical(r);
}
