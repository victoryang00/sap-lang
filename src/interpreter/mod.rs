use alloc::{boxed::Box, collections::BTreeMap, format, rc::Rc, string::String, vec, vec::Vec};
use core::{
    borrow::BorrowMut,
    cell::UnsafeCell,
    fmt::Debug,
    ops::{Deref, DerefMut},
};

pub mod type_checker;
use crate::parser::{
    expr::{CommentedExpr, Expr},
    ty::Type,
};

use self::{
    interpreter::{eval_expr, EvalContext, Value},
    std::add_std,
    type_checker::{type_check_expr, TypeCheckContext},
};

pub mod interpreter;
pub mod std;

pub struct Runner {
    type_check_context: Rc<UnsafeCell<TypeCheckContext>>,
    eval_context: Rc<UnsafeCell<EvalContext>>,
}
impl Debug for Runner {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("Runner")
            .field("type_check_context", unsafe {
                &*self.type_check_context.as_ref().get()
            })
            .field("eval_context", unsafe {
                &*self.eval_context.as_ref().get()
            })
            .finish()
    }
}

impl Runner {
    pub fn new() -> Self {
        let type_check_context = Rc::new(UnsafeCell::new(TypeCheckContext {
            parent: None,
            free_var: BTreeMap::new(),
            alias: BTreeMap::new(),
        }));
        let eval_context = Rc::new(UnsafeCell::new(EvalContext {
            parent: None,
            free_var: BTreeMap::new(),
        }));
        Self {
            type_check_context,
            eval_context,
        }
    }
    pub fn new_with_std() -> Self {
        let mut s = Self::new();
        add_std(&mut s);
        s
    }
    pub fn add_and_wrap_native_function(
        &mut self,
        args_counts: usize,
        name: String,
        ptr: unsafe extern "C" fn(usize, va: *mut *mut Value) -> UnsafeCell<Value>,
        type_info: Option<(Vec<Type>, Box<Type>)>,
    ) {
        let argss = (0..args_counts)
            .collect::<Vec<_>>()
            .iter()
            .map(|i| format!("_{:?}", i))
            .collect::<Vec<_>>();
        let nname = format!("_{}", name.clone());
        if let Some((a, b)) = type_info {
            unsafe { &mut *self.type_check_context.as_ref().get() }
                .free_var
                .insert(name.clone(), Type::Function(a, b));
        } else {
            unsafe { &mut *self.type_check_context.as_ref().get() }
                .free_var
                .insert(name.clone(), Type::Any);
        }
        unsafe { &mut *self.eval_context.as_ref().get() }
            .free_var
            .insert(
                nname.clone(),
                Rc::new(UnsafeCell::new(Value::NativeFunction(
                    nname.clone(),
                    argss.clone(),
                    BTreeMap::new(),
                    ptr,
                ))),
            );
        unsafe { &mut *self.eval_context.as_ref().get() }
            .free_var
            .insert(
                name.clone(),
                Rc::new(UnsafeCell::new(Value::Function(
                    argss.clone(),
                    BTreeMap::new(),
                    self.eval_context.clone(),
                    CommentedExpr::from_expr(Expr::Call(
                        Box::new(CommentedExpr::from_expr(Expr::Ident(vec![nname]))),
                        argss
                            .iter()
                            .map(|x| CommentedExpr::from_expr(Expr::Ident(vec![x.clone()])))
                            .collect(),
                    )),
                ))),
            );
    }
    pub fn type_check_and_run(
        &mut self,
        mut e: CommentedExpr,
    ) -> (
        Result<Type, &'static str>,
        Result<Rc<UnsafeCell<Value>>, &'static str>,
    ) {
        (
            type_check_expr(&mut e, self.type_check_context.clone()),
            eval_expr(&mut e, self.eval_context.clone()),
        )
    }
}
