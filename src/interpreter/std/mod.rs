use std::{
    cell::{RefCell, UnsafeCell},
    collections::HashMap,
    ffi::VaList,
    rc::Rc,
};

use crate::parser::{
    expr::{literal::Number, CommentedExpr, Expr},
    ty::Type,
};

use super::{
    interpreter::{EvalContext, Value},
    type_checker::TypeCheckContext,
};

struct NativeFunction {
    name: String,
    args: Vec<String>,
    filled_args: HashMap<String, Rc<UnsafeCell<Value>>>,
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

unsafe extern "C" fn add(args_count: usize, va: *mut *mut Value) -> UnsafeCell<Value> {
    let mut vec = Vec::from_raw_parts(va, args_count, 2);
    let v1 = &mut *vec[0];
    let v2 = &mut *vec[1];
    if let Value::Number(n1) = v1 {
        if let Value::Number(n2) = v2 {
            match n1 {
                crate::parser::expr::literal::Number::Integer(i1) => match n2 {
                    crate::parser::expr::literal::Number::Integer(i2) => {
                        UnsafeCell::new(Value::Number(Number::Integer(*i1 + *i2)))
                    }
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Number(Number::Floating(*i1 as f64 + *f2)))
                    }
                },
                crate::parser::expr::literal::Number::Floating(i2) => match n2 {
                    crate::parser::expr::literal::Number::Floating(f2) => {
                        UnsafeCell::new(Value::Number(Number::Floating(*i2 + *f2)))
                    }
                    crate::parser::expr::literal::Number::Integer(i3) => {
                        UnsafeCell::new(Value::Number(Number::Floating(*i2 + *i3 as f64)))
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

pub fn add_std(ty: &mut TypeCheckContext, ev: &mut EvalContext) {
    ty.free_var.insert(
        "_add".to_owned(),
        Type::Function(vec![Type::Number, Type::Number], Box::new(Type::Number)),
    );
    ty.free_var.insert(
        "add".to_owned(),
        Type::Function(vec![Type::Number, Type::Number], Box::new(Type::Number)),
    );
    ty.free_var.insert(
        "id".to_owned(),
        Type::Function(vec![Type::Any], Box::new(Type::Any)),
    );
    ev.free_var.insert(
        "id".to_owned(),
        Rc::new(UnsafeCell::new(Value::Function(
            vec!["i".to_owned()],
            HashMap::new(),
            Rc::new(UnsafeCell::new(ev.clone())),
            CommentedExpr::from_expr(Expr::Ident(vec!["i".to_owned()])),
        ))),
    );
    ev.free_var.insert(
        "_add".to_owned(),
        Rc::new(UnsafeCell::new(Value::NativeFunction(
            "_add".to_owned(),
            vec!["a".to_owned(), "b".to_owned()],
            HashMap::new(),
            add,
        ))),
    );
    ev.free_var.insert(
        "add".to_owned(),
        Rc::new(UnsafeCell::new(Value::Function(
            vec!["a".to_owned(), "b".to_owned()],
            HashMap::new(),
            Rc::new(UnsafeCell::new(ev.clone())),
            CommentedExpr::from_expr(Expr::Call(
                Box::new(CommentedExpr::from_expr(Expr::Ident(vec![
                    "_add".to_owned()
                ]))),
                vec![
                    CommentedExpr::from_expr(Expr::Ident(vec!["a".to_owned()])),
                    CommentedExpr::from_expr(Expr::Ident(vec!["b".to_owned()])),
                ],
            )),
        ))),
    );
}
