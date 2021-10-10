mod numerical;

use std::cell::UnsafeCell;
use std::{
    boxed::Box,
    collections::BTreeMap,
    format,
    rc::Rc,
    string::{String, ToString},
    vec,
    vec::Vec,
};

use crate::parser::{expr::literal::Number, ty::Type};

use self::numerical::add_std_numerical;

use super::{evaluator::value::Value, SapState};

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

unsafe extern "C" fn range(args_count: usize, va: *mut *mut Value) -> UnsafeCell<Value> {
    let mut vec = Vec::from_raw_parts(va, args_count, 2);
    let v1 = &mut *vec[0];
    let v2 = &mut *vec[1];
    if let Value::Number(n1) = v1 {
        if let Value::Number(n2) = v2 {
            match n1 {
                crate::parser::expr::literal::Number::Integer(i1) => match n2 {
                    crate::parser::expr::literal::Number::Integer(i2) => {
                        let mut v = vec![];
                        for i in *i1..*i2 {
                            v.push(Rc::new(UnsafeCell::new(Value::Number(Number::Integer(i)))))
                        }
                        UnsafeCell::new(Value::Array(v))
                    }
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        let mut v = vec![];
                        for i in *i1..(f2.floor() as i128) {
                            v.push(Rc::new(UnsafeCell::new(Value::Number(Number::Integer(i)))))
                        }
                        UnsafeCell::new(Value::Array(v))
                    }
                },
                crate::parser::expr::literal::Number::Floating(f1) => match n2 {
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        let mut v = vec![];
                        for i in f1.floor() as i128..f2.floor() as i128 {
                            v.push(Rc::new(UnsafeCell::new(Value::Number(Number::Integer(i)))))
                        }
                        UnsafeCell::new(Value::Array(v))
                    }
                    crate::parser::expr::literal::Number::Integer(i3) => {
                        let mut v = vec![];
                        for i in f1.floor() as i128..*i3 {
                            v.push(Rc::new(UnsafeCell::new(Value::Number(Number::Integer(i)))))
                        }
                        UnsafeCell::new(Value::Array(v))
                    }
                },
            }
        } else {
            UnsafeCell::new(Value::Error(format!(
                "TYPE MISMACHED IN RUNTIME with {:?}",
                v2
            )))
        }
    } else {
        UnsafeCell::new(Value::Error(format!(
            "TYPE MISMACHED IN RUNTIME with {:?}",
            v1
        )))
    }
}

unsafe extern "C" fn cons(args_count: usize, va: *mut *mut Value) -> UnsafeCell<Value> {
    let mut vec = Vec::from_raw_parts(va, args_count, 2);
    let v1 = &mut *vec[1];
    let v2 = &mut *vec[0];
    match v1 {
        Value::Array(a) => match v2 {
            Value::Array(b) => UnsafeCell::new(Value::Array({
                let mut r = a.clone();
                r.append(&mut b.clone());
                r
            })),
            _ => panic!("FUCK RUNTIME"),
        },
        v => {
            let v = Rc::new(UnsafeCell::new(v.clone()));
            let a = vec![v];
            match v2 {
                Value::Array(b) => UnsafeCell::new(Value::Array({
                    let mut r = a.clone();
                    r.append(&mut b.clone());
                    r
                })),
                _ => panic!("FUCK RUNTIME"),
            }
        }
    }
}

unsafe extern "C" fn uncons(args_count: usize, va: *mut *mut Value) -> UnsafeCell<Value> {
    let mut vec = Vec::from_raw_parts(va, args_count, 2);
    let v1 = &mut *vec[0];
    match v1 {
        Value::Array(a) => UnsafeCell::new(Value::Array(vec![
            Rc::new(UnsafeCell::new(unsafe { &*a[0].get() }.clone())),
            Rc::new(UnsafeCell::new(Value::Array(
                a[1..]
                    .iter()
                    .map(|x| Rc::new(UnsafeCell::new(unsafe { &*x.get() }.clone())))
                    .collect::<Vec<_>>(),
            ))),
        ])),
        Value::Object(_) => todo!(),
        _ => todo!(),
    }
}

unsafe extern "C" fn is_empty(args_count: usize, va: *mut *mut Value) -> UnsafeCell<Value> {
    let mut vec = Vec::from_raw_parts(va, args_count, 2);
    let v1 = &mut *vec[0];
    match v1 {
        Value::Array(a) => UnsafeCell::new(Value::Bool(a.is_empty())),
        Value::Object(_) => todo!(),
        _ => todo!(),
    }
}

pub fn add_std(r: &mut SapState) {
    //numerical
    add_std_numerical(r);
    r.add_and_wrap_native_function(
        2,
        "range".to_string(),
        range,
        Some((vec![Type::Number, Type::Number], Box::new(Type::Array))),
    );
    r.add_and_wrap_native_function(
        2,
        "cons".to_string(),
        cons,
        Some((vec![Type::Any, Type::Array], Box::new(Type::Array))),
    );
    r.add_and_wrap_native_function(
        1,
        "uncons".to_string(),
        uncons,
        Some((vec![Type::Any], Box::new(Type::Array))),
    );
    r.add_and_wrap_native_function(
        1,
        "is_empty".to_string(),
        is_empty,
        Some((vec![Type::Any], Box::new(Type::Bool))),
    );
    r.add_and_wrap_native_function(
        1,
        "id".to_string(),
        id,
        Some((vec![Type::Any], Box::new(Type::Any))),
    );
}
