use core::{cell::UnsafeCell, ops::*};

use alloc::{boxed::Box, fmt::format, format, string::ToString, vec, vec::Vec};

use crate::{
    interpreter::{interpreter::Value, Runner},
    parser::{expr::literal::Number, ty::Type},
};

unsafe extern "C" fn lt(args_count: usize, va: *mut *mut Value) -> UnsafeCell<Value> {
    let mut vec = Vec::from_raw_parts(va, args_count, 2);
    let v1 = &mut *vec[1];
    let v2 = &mut *vec[0];
    if let Value::Number(n1) = v1 {
        if let Value::Number(n2) = v2 {
            match n1 {
                crate::parser::expr::literal::Number::Integer(i1) => match n2 {
                    crate::parser::expr::literal::Number::Integer(i2) => {
                        UnsafeCell::new(Value::Bool((*i1).lt(i2)))
                    }
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Bool((*i1 as f64).lt(f2)))
                    }
                },
                crate::parser::expr::literal::Number::Floating(i2) => match n2 {
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Bool((*i2).lt(f2)))
                    }
                    crate::parser::expr::literal::Number::Integer(i3) => {
                        UnsafeCell::new(Value::Bool((*i2).lt(&(*i3 as f64))))
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

unsafe extern "C" fn gt(args_count: usize, va: *mut *mut Value) -> UnsafeCell<Value> {
    let mut vec = Vec::from_raw_parts(va, args_count, 2);
    let v1 = &mut *vec[1];
    let v2 = &mut *vec[0];
    if let Value::Number(n1) = v1 {
        if let Value::Number(n2) = v2 {
            match n1 {
                crate::parser::expr::literal::Number::Integer(i1) => match n2 {
                    crate::parser::expr::literal::Number::Integer(i2) => {
                        UnsafeCell::new(Value::Bool((*i1).gt(i2)))
                    }
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Bool((*i1 as f64).gt(f2)))
                    }
                },
                crate::parser::expr::literal::Number::Floating(i2) => match n2 {
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Bool((*i2).gt(f2)))
                    }
                    crate::parser::expr::literal::Number::Integer(i3) => {
                        UnsafeCell::new(Value::Bool((*i2).gt(&(*i3 as f64))))
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

unsafe extern "C" fn le(args_count: usize, va: *mut *mut Value) -> UnsafeCell<Value> {
    let mut vec = Vec::from_raw_parts(va, args_count, 2);
    let v1 = &mut *vec[1];
    let v2 = &mut *vec[0];
    if let Value::Number(n1) = v1 {
        if let Value::Number(n2) = v2 {
            match n1 {
                crate::parser::expr::literal::Number::Integer(i1) => match n2 {
                    crate::parser::expr::literal::Number::Integer(i2) => {
                        UnsafeCell::new(Value::Bool((*i1).le(i2)))
                    }
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Bool((*i1 as f64).le(f2)))
                    }
                },
                crate::parser::expr::literal::Number::Floating(i2) => match n2 {
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Bool((*i2).le(f2)))
                    }
                    crate::parser::expr::literal::Number::Integer(i3) => {
                        UnsafeCell::new(Value::Bool((*i2).le(&(*i3 as f64))))
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

unsafe extern "C" fn ge(args_count: usize, va: *mut *mut Value) -> UnsafeCell<Value> {
    let mut vec = Vec::from_raw_parts(va, args_count, 2);
    let v1 = &mut *vec[1];
    let v2 = &mut *vec[0];
    if let Value::Number(n1) = v1 {
        if let Value::Number(n2) = v2 {
            match n1 {
                crate::parser::expr::literal::Number::Integer(i1) => match n2 {
                    crate::parser::expr::literal::Number::Integer(i2) => {
                        UnsafeCell::new(Value::Bool((*i1).ge(i2)))
                    }
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Bool((*i1 as f64).ge(f2)))
                    }
                },
                crate::parser::expr::literal::Number::Floating(i2) => match n2 {
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Bool((*i2).ge(f2)))
                    }
                    crate::parser::expr::literal::Number::Integer(i3) => {
                        UnsafeCell::new(Value::Bool((*i2).ge(&(*i3 as f64))))
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

unsafe extern "C" fn eq(args_count: usize, va: *mut *mut Value) -> UnsafeCell<Value> {
    let mut vec = Vec::from_raw_parts(va, args_count, 2);
    let v1 = &mut *vec[1];
    let v2 = &mut *vec[0];
    if let Value::Number(n1) = v1 {
        if let Value::Number(n2) = v2 {
            match n1 {
                crate::parser::expr::literal::Number::Integer(i1) => match n2 {
                    crate::parser::expr::literal::Number::Integer(i2) => {
                        UnsafeCell::new(Value::Bool((*i1).eq(i2)))
                    }
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Bool((*i1 as f64).eq(f2)))
                    }
                },
                crate::parser::expr::literal::Number::Floating(i2) => match n2 {
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Bool((*i2).eq(f2)))
                    }
                    crate::parser::expr::literal::Number::Integer(i3) => {
                        UnsafeCell::new(Value::Bool((*i2).eq(&(*i3 as f64))))
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

unsafe extern "C" fn ne(args_count: usize, va: *mut *mut Value) -> UnsafeCell<Value> {
    let mut vec = Vec::from_raw_parts(va, args_count, 2);
    let v1 = &mut *vec[1];
    let v2 = &mut *vec[0];
    if let Value::Number(n1) = v1 {
        if let Value::Number(n2) = v2 {
            match n1 {
                crate::parser::expr::literal::Number::Integer(i1) => match n2 {
                    crate::parser::expr::literal::Number::Integer(i2) => {
                        UnsafeCell::new(Value::Bool((*i1).ne(i2)))
                    }
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Bool((*i1 as f64).ne(f2)))
                    }
                },
                crate::parser::expr::literal::Number::Floating(i2) => match n2 {
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Bool((*i2).ne(f2)))
                    }
                    crate::parser::expr::literal::Number::Integer(i3) => {
                        UnsafeCell::new(Value::Bool((*i2).ne(&(*i3 as f64))))
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

unsafe extern "C" fn add(args_count: usize, va: *mut *mut Value) -> UnsafeCell<Value> {
    let mut vec = Vec::from_raw_parts(va, args_count, 2);
    let v1 = &mut *vec[1];
    let v2 = &mut *vec[0];
    if let Value::Number(n1) = v1 {
        if let Value::Number(n2) = v2 {
            match n1 {
                crate::parser::expr::literal::Number::Integer(i1) => match n2 {
                    crate::parser::expr::literal::Number::Integer(i2) => {
                        UnsafeCell::new(Value::Number(Number::Integer((*i1).add(*i2))))
                    }
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Number(Number::Floating((*i1 as f64).add(*f2))))
                    }
                },
                crate::parser::expr::literal::Number::Floating(f1) => match n2 {
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Number(Number::Floating((*f1).add(*f2))))
                    }
                    crate::parser::expr::literal::Number::Integer(i3) => {
                        UnsafeCell::new(Value::Number(Number::Floating((*f1).add(*i3 as f64))))
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

unsafe extern "C" fn sub(args_count: usize, va: *mut *mut Value) -> UnsafeCell<Value> {
    let mut vec = Vec::from_raw_parts(va, args_count, 2);
    let v1 = &mut *vec[1];
    let v2 = &mut *vec[0];
    if let Value::Number(n1) = v1 {
        if let Value::Number(n2) = v2 {
            match n1 {
                crate::parser::expr::literal::Number::Integer(i1) => match n2 {
                    crate::parser::expr::literal::Number::Integer(i2) => {
                        UnsafeCell::new(Value::Number(Number::Integer((*i1).sub(*i2))))
                    }
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Number(Number::Floating((*i1 as f64).sub(*f2))))
                    }
                },
                crate::parser::expr::literal::Number::Floating(f1) => match n2 {
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Number(Number::Floating((*f1).sub(*f2))))
                    }
                    crate::parser::expr::literal::Number::Integer(i3) => {
                        UnsafeCell::new(Value::Number(Number::Floating((*f1).sub(*i3 as f64))))
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

unsafe extern "C" fn mul(args_count: usize, va: *mut *mut Value) -> UnsafeCell<Value> {
    let mut vec = Vec::from_raw_parts(va, args_count, 2);
    let v1 = &mut *vec[1];
    let v2 = &mut *vec[0];
    if let Value::Number(n1) = v1 {
        if let Value::Number(n2) = v2 {
            match n1 {
                crate::parser::expr::literal::Number::Integer(i1) => match n2 {
                    crate::parser::expr::literal::Number::Integer(i2) => {
                        UnsafeCell::new(Value::Number(Number::Integer((*i1).mul(*i2))))
                    }
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Number(Number::Floating((*i1 as f64).mul(*f2))))
                    }
                },
                crate::parser::expr::literal::Number::Floating(f1) => match n2 {
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Number(Number::Floating((*f1).mul(*f2))))
                    }
                    crate::parser::expr::literal::Number::Integer(i3) => {
                        UnsafeCell::new(Value::Number(Number::Floating((*f1).mul(*i3 as f64))))
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

unsafe extern "C" fn div(args_count: usize, va: *mut *mut Value) -> UnsafeCell<Value> {
    let mut vec = Vec::from_raw_parts(va, args_count, 2);
    let v1 = &mut *vec[1];
    let v2 = &mut *vec[0];
    if let Value::Number(n1) = v1 {
        if let Value::Number(n2) = v2 {
            match n1 {
                crate::parser::expr::literal::Number::Integer(i1) => match n2 {
                    crate::parser::expr::literal::Number::Integer(i2) => {
                        UnsafeCell::new(Value::Number(Number::Integer((*i1).div(*i2))))
                    }
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Number(Number::Floating((*i1 as f64).div(*f2))))
                    }
                },
                crate::parser::expr::literal::Number::Floating(f1) => match n2 {
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Number(Number::Floating((*f1).div(*f2))))
                    }
                    crate::parser::expr::literal::Number::Integer(i3) => {
                        UnsafeCell::new(Value::Number(Number::Floating((*f1).div(*i3 as f64))))
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

unsafe extern "C" fn rem(args_count: usize, va: *mut *mut Value) -> UnsafeCell<Value> {
    let mut vec = Vec::from_raw_parts(va, args_count, 2);
    let v1 = &mut *vec[1];
    let v2 = &mut *vec[0];
    if let Value::Number(n1) = v1 {
        if let Value::Number(n2) = v2 {
            match n1 {
                crate::parser::expr::literal::Number::Integer(i1) => match n2 {
                    crate::parser::expr::literal::Number::Integer(i2) => {
                        UnsafeCell::new(Value::Number(Number::Integer((*i1).rem(*i2))))
                    }
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Number(Number::Floating((*i1 as f64).rem(*f2))))
                    }
                },
                crate::parser::expr::literal::Number::Floating(f1) => match n2 {
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Number(Number::Floating((*f1).rem(*f2))))
                    }
                    crate::parser::expr::literal::Number::Integer(i3) => {
                        UnsafeCell::new(Value::Number(Number::Floating((*f1).rem(*i3 as f64))))
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

pub(crate) fn add_std_numerical(r: &mut Runner) {
    r.add_and_wrap_native_function(
        2,
        "lt".to_string(),
        lt,
        Some((vec![Type::Number, Type::Number], Box::new(Type::Bool))),
    );
    r.add_and_wrap_native_function(
        2,
        "gt".to_string(),
        gt,
        Some((vec![Type::Number, Type::Number], Box::new(Type::Bool))),
    );
    r.add_and_wrap_native_function(
        2,
        "le".to_string(),
        le,
        Some((vec![Type::Number, Type::Number], Box::new(Type::Bool))),
    );
    r.add_and_wrap_native_function(
        2,
        "ge".to_string(),
        ge,
        Some((vec![Type::Number, Type::Number], Box::new(Type::Bool))),
    );
    r.add_and_wrap_native_function(
        2,
        "eq".to_string(),
        eq,
        Some((vec![Type::Number, Type::Number], Box::new(Type::Bool))),
    );
    r.add_and_wrap_native_function(
        2,
        "ne".to_string(),
        ne,
        Some((vec![Type::Number, Type::Number], Box::new(Type::Bool))),
    );
    r.add_and_wrap_native_function(
        2,
        "add".to_string(),
        add,
        Some((vec![Type::Number, Type::Number], Box::new(Type::Number))),
    );
    r.add_and_wrap_native_function(
        2,
        "sub".to_string(),
        sub,
        Some((vec![Type::Number, Type::Number], Box::new(Type::Number))),
    );
    r.add_and_wrap_native_function(
        2,
        "mul".to_string(),
        mul,
        Some((vec![Type::Number, Type::Number], Box::new(Type::Number))),
    );
    r.add_and_wrap_native_function(
        2,
        "div".to_string(),
        div,
        Some((vec![Type::Number, Type::Number], Box::new(Type::Number))),
    );
    r.add_and_wrap_native_function(
        2,
        "rem".to_string(),
        rem,
        Some((vec![Type::Number, Type::Number], Box::new(Type::Number))),
    );
}
