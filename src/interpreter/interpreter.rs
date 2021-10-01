use std::{
    borrow::{Borrow, BorrowMut},
    cell::UnsafeCell,
    collections::{BTreeMap, HashMap},
    ops::{Deref, DerefMut},
    rc::Rc,
};

use crate::parser::expr::{literal::Number, Expr};

#[derive(Debug, Clone)]
pub struct EvalContext {
    pub(crate) parent: Option<Rc<UnsafeCell<EvalContext>>>,
    pub(crate) free_var: HashMap<String, Rc<UnsafeCell<Value>>>,
}
impl EvalContext {
    fn new_with(context: Rc<UnsafeCell<EvalContext>>) -> Self {
        Self {
            parent: Some(context),
            free_var: HashMap::new(),
        }
    }

    fn get_free_var(&self, k: &String) -> Option<Rc<UnsafeCell<Value>>> {
        if self.free_var.contains_key(k) {
            Some(self.free_var[k].clone())
        } else {
            if let Some(p) = self.parent.clone() {
                (unsafe { &*p.get() }).get_free_var(k)
            } else {
                None
            }
        }
    }
}
#[derive(Clone)]
pub enum Value {
    Any(Rc<UnsafeCell<Value>>),
    // AnyArgsList,
    Number(Number),
    String(String),
    Bool(bool),
    // entrys, current call status, eval context, expr
    Function(
        Vec<String>,
        HashMap<String, Rc<UnsafeCell<Value>>>,
        Rc<UnsafeCell<EvalContext>>,
        Expr,
    ),
    NativeFunction(
        String,
        Vec<String>,
        HashMap<String, Rc<UnsafeCell<Value>>>,
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
impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Any(arg0) => f.debug_tuple("Any").field(arg0).finish(),
            Self::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::Function(arg0, arg1, arg2, arg3) => f
                .debug_tuple("Function")
                .field(arg0)
                .field(arg1)
                .field(arg3)
                .finish(),
            Self::NativeFunction(arg0, arg1, arg2, arg3) => f
                .debug_tuple("NativeFunction")
                .field(arg0)
                .field(arg1)
                .field(arg2)
                .field(arg3)
                .finish(),
            Self::Array(arg0) => f.debug_tuple("Array").field(arg0).finish(),
            Self::Object(arg0) => f.debug_tuple("Object").field(arg0).finish(),
            Self::Enum(arg0) => f.debug_tuple("Enum").field(arg0).finish(),
            Self::Null => write!(f, "Null"),
            Self::Error(arg0) => f.debug_tuple("Error").field(arg0).finish(),
        }
    }
}

pub unsafe fn call_func_with_expr(
    f: &mut Value,
    mut args: Vec<Expr>,
    context: Rc<UnsafeCell<EvalContext>>,
) -> Result<Rc<UnsafeCell<Value>>, &'static str> {
    let mut v = vec![];
    for mut a in &mut args {
        let a = eval_expr(a, context.clone())?;
        v.push(a);
    }
    if let Value::Function(fargs, h, ct, fb) = f {
        if v.len() > fargs.len() {
            for i in 0..fargs.len() {
                h.insert(fargs[h.len()].clone(), v[i].clone());
            }
            for (k, v) in h.clone() {
                (&mut *ct.get()).free_var.insert(k.clone(), v.clone());
            }
            let mut r = eval_expr(fb, ct.clone())?;
            let mut r = r.as_ref();
            let mut r = unsafe { &mut *r.get() };
            call_func_with_expr(
                r,
                args[h.len()..]
                    .iter()
                    .map(|x| x.clone())
                    .collect::<Vec<_>>(),
                context,
            )
        } else if v.len() < fargs.len() {
            for i in 0..v.len() {
                h.insert(fargs[h.len()].clone(), v[i].clone());
            }
            Ok(Rc::new(UnsafeCell::new(f.clone())))
        } else {
            let hh = h.clone();
            for i in 0..v.len() {
                h.insert(fargs[h.len()].clone(), v[i].clone());
            }
            for (k, v) in h.clone() {
                (&mut *ct.get()).free_var.insert(k.clone(), v.clone());
            }
            let r = eval_expr(fb, ct.clone());
            *h = hh;
            r
        }
    } else if let Value::NativeFunction(name, fargs, h, ff) = f {
        if v.len() > fargs.len() {
            for i in 0..fargs.len() {
                h.insert(fargs[h.len()].clone(), v[i].clone());
            }
            let mut vv = vec![];
            for i in fargs {
                vv.push(h.get(i).unwrap().clone().as_ref().get())
            }
            let (p, len, _) = vv.into_raw_parts();
            let mut r = Rc::new(ff(len, p));
            let mut r = r.as_ref();
            let mut r = unsafe { &mut *r.get() };
            call_func_with_expr(
                r,
                args[h.len()..]
                    .iter()
                    .map(|x| x.clone())
                    .collect::<Vec<_>>(),
                context,
            )
        } else if v.len() < fargs.len() {
            for i in 0..v.len() {
                h.insert(fargs[h.len()].clone(), v[i].clone());
            }
            Ok(Rc::new(UnsafeCell::new(f.clone())))
        } else {
            let mut hh = h.clone();
            let mut vv = vec![];
            for i in 0..fargs.len() {
                h.insert(fargs[h.len()].clone(), v[i].clone());
            }
            for i in fargs {
                vv.push(h.get(i).unwrap().clone().as_ref().get())
            }
            let (p, len, _) = vv.into_raw_parts();
            let mut r = Rc::new(ff(len, p));
            *h = hh;
            Ok(r)
        }
    } else {
        panic!("DID YOU PASS TYPE CHECKER RIGHT?")
    }
}

pub fn eval_expr(
    e: &mut Expr,
    context: Rc<UnsafeCell<EvalContext>>,
) -> Result<Rc<UnsafeCell<Value>>, &'static str> {
    match e {
        Expr::Quoted(e) => eval_expr(e, context),
        Expr::Block(es) => {
            let ct = Rc::new(UnsafeCell::new(EvalContext::new_with(context.clone())));
            let mut r = Rc::new(UnsafeCell::new(Value::Null));
            for e in es {
                r = eval_expr(e, ct.clone())?;
            }
            Ok(r)
        }
        Expr::Literal(l) => match l {
            crate::parser::expr::literal::Literal::Null => {
                Ok(Rc::new(UnsafeCell::new(Value::Null)))
            }
            crate::parser::expr::literal::Literal::Bool(b) => {
                Ok(Rc::new(UnsafeCell::new(Value::Bool(*b))))
            }
            // TODO: formated string
            crate::parser::expr::literal::Literal::String(s) => {
                Ok(Rc::new(UnsafeCell::new(Value::String(s.clone()))))
            }
            crate::parser::expr::literal::Literal::Number(n) => {
                Ok(Rc::new(UnsafeCell::new(Value::Number(n.clone()))))
            }
        },
        Expr::Ident(i) => {
            if i.len() > 1 {
                unimplemented!()
            } else {
                if let Some(v) = unsafe { &mut *context.get() }.get_free_var(i.last().unwrap()) {
                    Ok(v.clone())
                } else {
                    Err("NO VAR")
                }
            }
        }
        Expr::Array(a) => {
            let mut v = vec![];
            for a in a {
                let a = eval_expr(a, context.clone())?;
                v.push(a);
            }
            Ok(Rc::new(UnsafeCell::new(Value::Array(v))))
        }
        Expr::Object(es) => {
            let mut b = BTreeMap::new();
            for (k, v) in es {
                b.insert(k.clone(), eval_expr(v, context.clone()).unwrap());
            }
            Ok(Rc::new(UnsafeCell::new(Value::Object(b))))
        }
        Expr::Closure(a, b, c) => {
            let ct = Rc::new(UnsafeCell::new(EvalContext::new_with(context.clone())));
            Ok(Rc::new(UnsafeCell::new(Value::Function(
                a.iter().map(|(s, _)| s.clone()).collect::<Vec<_>>(),
                HashMap::new(),
                ct,
                *(c.clone()),
            ))))
        }
        Expr::If(a, b, c) => {
            let aaa = eval_expr(a, context.clone())?;
            let aaa = aaa.as_ref();
            let mut aaa = unsafe { &mut *aaa.get() };
            if let Value::Bool(bb) = aaa {
                if bb.clone() {
                    eval_expr(b, context.clone())
                } else {
                    eval_expr(c, context.clone())
                }
            } else {
                panic!("DID YOU USE TYPE CHECKER?!")
            }
        }
        Expr::MultiIf(a, b) => todo!(),
        Expr::For(a, b, c) => {
            todo!();
            Ok(Rc::new(UnsafeCell::new(Value::Null)))
        }
        Expr::Call(func, args) => {
            let f = eval_expr(func, context.clone())?;
            let f = f.as_ref();
            let mut f = unsafe { &mut *f.get() };
            unsafe { call_func_with_expr(f, args.clone(), context) }
        }
        Expr::ErrorHandle(e) => match eval_expr(e.borrow_mut(), context) {
            Ok(e) => Ok(e),
            Err(_) => Ok(Rc::new(UnsafeCell::new(Value::Null))),
        },
        // TODO: FUCK ME
        Expr::Bind(a, b) => {
            let aaa = eval_expr(a, context.clone())?;
            let aaa = aaa.as_ref();
            let mut aaa = unsafe { &mut *aaa.get() };

            let bbb = eval_expr(b, context.clone())?;
            let bbb = bbb.as_ref();
            let mut bbb = unsafe { &mut *bbb.get() };
            match bbb {
                Value::Function(a, hs, ct, e) => match aaa.clone() {
                    Value::Function(aa, mut bb, cct, ee) => {
                        todo!()
                    }
                    nonf => {
                        if a.len() < 1 {
                            Err("no enought args")
                        } else {
                            unsafe { call_func_with_expr(bbb, vec![*b.clone()], context) }
                        }
                    }
                },
                _ => Err("not bindable"),
            }
        }
        // TODO: elab index
        Expr::Index(a, b) => {
            let aaa = eval_expr(a, context.clone())?;
            let aaa = aaa.as_ref();
            let mut aaa = unsafe { &mut *aaa.get() };

            let bbb = eval_expr(b, context.clone())?;
            let bbb = bbb.as_ref();
            let mut bbb = unsafe { &mut *bbb.get() };
            if let Value::Array(a) = aaa {
                if let Value::Number(n) = bbb {
                    let n = match n {
                        Number::Integer(i) => *i as usize,
                        Number::Floating(f) => (*f).floor() as usize,
                    };
                    Ok(a[n].clone())
                } else {
                    panic!("TYPE CHECKER?!")
                }
            } else if let Value::Object(o) = aaa {
                if let Value::String(s) = bbb {
                    Ok(o[s].clone())
                } else {
                    panic!("TYPE CHECKER!!!!!!")
                }
            } else {
                panic!("TYPE...")
            }
        }
        Expr::Assign(a, b) => {
            let mut aaa = match eval_expr(a, context.clone()) {
                Ok(aaa) => {
                    let mut bbb = eval_expr(b, context.clone())?;
                    *unsafe { &mut *aaa.get() } = unsafe { &mut *bbb.get() }.clone();
                    Ok(bbb)
                }
                Err(_) => {
                    let mut bbb = eval_expr(b, context.clone())?;
                    if let box Expr::Ident(aa) = a {
                        if aa.len() > 1 {
                            unimplemented!()
                        } else {
                            unsafe { &mut *context.as_ref().get() }
                                .free_var
                                .insert(aa.last().unwrap().clone(), bbb.clone());
                        }
                        Ok(bbb)
                    } else {
                        panic!("FUCK ME")
                    }
                }
            };
            aaa
        }
        Expr::SpecifyTyped(ex, ty) => eval_expr(ex.borrow_mut(), context),
    }
}
