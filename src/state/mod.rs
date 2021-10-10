pub mod type_checker;
use ::std::{cell::UnsafeCell, collections::BTreeMap, rc::Rc};

use crate::{
    parser::{
        expr::{CommentedExpr, Expr},
        top_level::TopLevel,
        ty::Type,
    },
    state::{evaluator::EvaledImports, type_checker::TypeCheckedImports},
};

use self::{
    evaluator::{eval_toplevel, value::Value, EvalContext},
    std::add_std,
    type_checker::{type_check_toplevel, TypeCheckContext},
};

pub mod evaluator;
pub mod std;

pub struct SapState {
    type_check_context: Rc<UnsafeCell<TypeCheckContext>>,
    eval_context: Rc<UnsafeCell<EvalContext>>,
}
impl ::std::fmt::Debug for SapState {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
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

impl SapState {
    pub fn new() -> Self {
        let type_check_context = Rc::new(UnsafeCell::new(TypeCheckContext {
            type_check_parent_scope: None,
            type_check_current_scope_variables: BTreeMap::new(),
            type_check_current_scope_type_alias: BTreeMap::new(),
            imports: Rc::new(UnsafeCell::new(TypeCheckedImports {
                imports: BTreeMap::new(),
            })),
            current_scope_import_alias: BTreeMap::new(),
        }));
        let eval_context = Rc::new(UnsafeCell::new(EvalContext {
            imports: Rc::new(UnsafeCell::new(EvaledImports {
                imports: BTreeMap::new(),
            })),
            current_scope_import_alias: BTreeMap::new(),
            eval_parent_scope: None,
            eval_current_scope_variables: BTreeMap::new(),
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
                .type_check_current_scope_variables
                .insert(name.clone(), Type::Function(a, b));
        } else {
            unsafe { &mut *self.type_check_context.as_ref().get() }
                .type_check_current_scope_variables
                .insert(name.clone(), Type::Any);
        }
        unsafe { &mut *self.eval_context.as_ref().get() }
            .eval_current_scope_variables
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
            .eval_current_scope_variables
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
        mut e: TopLevel,
    ) -> (
        Result<Option<Type>, &'static str>,
        Result<Rc<UnsafeCell<Value>>, &'static str>,
    ) {
        (
            type_check_toplevel(&mut e, self.type_check_context.clone()),
            eval_toplevel(&mut e, self.eval_context.clone()),
        )
    }
}
