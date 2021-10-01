use std::{
    borrow::{Borrow, BorrowMut},
    cell::UnsafeCell,
    collections::{BTreeMap, HashMap},
    ops::{Deref, DerefMut},
    rc::Rc,
};

use crate::parser::expr::{
    literal::{Literal, Number},
    reverse_bind, Expr,
};

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

pub unsafe fn call_func_with_expr(
    f: Value,
    mut args: Vec<Expr>,
    context: Rc<UnsafeCell<EvalContext>>,
) -> Result<Rc<UnsafeCell<Value>>, &'static str> {
    let mut v = vec![];
    for mut a in &mut args {
        let a = eval_expr(a, context.clone())?;
        v.push(a);
    }
    if let Value::Function(fargs, h, ct, fb) = &mut f.clone() {
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
                r.clone(),
                args[h.len()..]
                    .iter()
                    .map(|x| x.clone())
                    .collect::<Vec<_>>(),
                context,
            )
        } else if v.len() < fargs.len() {
            let mut ctx = EvalContext::new_with(context);
            ctx.free_var
                .insert("@func".to_owned(), Rc::new(UnsafeCell::new(f)));
            let mut expr = Expr::Closure(
                fargs[v.len()..]
                    .iter()
                    .map(|x| (x.clone(), None))
                    .collect::<Vec<_>>(),
                None,
                Box::new(Expr::Call(
                    Box::new(Expr::Ident(vec!["@func".to_owned()])),
                    {
                        let mut vv = args;
                        vv.append(
                            &mut fargs[v.len()..]
                                .iter()
                                .map(|x| Expr::Ident(vec![x.clone()]))
                                .collect::<Vec<_>>(),
                        );
                        vv
                    },
                )),
            );
            eval_expr(&mut expr, Rc::new(UnsafeCell::new(ctx)))
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
    } else if let Value::NativeFunction(name, fargs, mut h, ff) = f.clone() {
        if v.len() > fargs.len() {
            let mut hh = h.clone();
            let mut vv = vec![];
            for i in 0..fargs.len() {
                h.insert(fargs[h.len()].clone(), v[i].clone());
            }
            for i in fargs {
                vv.push(h.get(&i).unwrap().clone().as_ref().get())
            }
            let (p, len, _) = vv.into_raw_parts();
            let mut r = Rc::new(ff(len, p));
            let mut r = r.as_ref();
            let mut r = unsafe { &mut *r.get() };
            call_func_with_expr(
                r.clone(),
                args[h.len()..]
                    .iter()
                    .map(|x| x.clone())
                    .collect::<Vec<_>>(),
                context,
            )
        } else if v.len() < fargs.len() {
            let mut ctx = EvalContext::new_with(context);
            ctx.free_var
                .insert("@func".to_owned(), Rc::new(UnsafeCell::new(f)));
            let mut expr = Expr::Closure(
                fargs[v.len()..]
                    .iter()
                    .map(|x| (x.clone(), None))
                    .collect::<Vec<_>>(),
                None,
                Box::new(Expr::Call(
                    Box::new(Expr::Ident(vec!["@func".to_owned()])),
                    {
                        let mut vv = args;
                        vv.append(
                            &mut fargs[v.len()..]
                                .iter()
                                .map(|x| Expr::Ident(vec![x.clone()]))
                                .collect::<Vec<_>>(),
                        );
                        vv
                    },
                )),
            );
            eval_expr(&mut expr, Rc::new(UnsafeCell::new(ctx)))
        } else {
            let mut hh = h.clone();
            let mut vv = vec![];
            for i in 0..fargs.len() {
                h.insert(fargs[h.len()].clone(), v[i].clone());
            }
            for i in fargs {
                vv.push(h.get(&i).unwrap().clone().as_ref().get())
            }
            let (p, len, _) = vv.into_raw_parts();
            let mut r = Rc::new(ff(len, p));
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
    // println!("trace: expr: {:?} context: {:?}", e, unsafe {
    //     &*context.as_ref().get()
    // });
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
            unsafe { call_func_with_expr(f.clone(), args.clone(), context) }
        }
        Expr::ErrorHandle(e) => match eval_expr(e.borrow_mut(), context) {
            Ok(e) => Ok(e),
            Err(_) => Ok(Rc::new(UnsafeCell::new(Value::Null))),
        },
        // TODO: FUCK ME
        Expr::Bind(a, b) => {
            let aaa = eval_expr(a, context.clone())?;
            let raaa = aaa.as_ref();
            let mut raaa = unsafe { &mut *aaa.get() };

            let bbb = eval_expr(b, context.clone())?;
            let rbbb = bbb.as_ref();
            let mut rbbb = unsafe { &mut *bbb.get() };
            match rbbb {
                Value::Function(a1, hs, ct, e) => match raaa.clone() {
                    Value::Function(a2, mut bb, cct, ee) => {
                        let new_ctx = Rc::new(UnsafeCell::new(EvalContext::new_with(context)));
                        unsafe {
                            { &mut *new_ctx.as_ref().get() }
                                .free_var
                                .insert("@f".to_owned(), aaa);
                            { &mut *new_ctx.as_ref().get() }
                                .free_var
                                .insert("@g".to_owned(), bbb);
                            let mut expr = Expr::Call(
                                Box::new(Expr::Closure(
                                    a2.iter().map(|x| (x.clone(), None)).collect::<Vec<_>>(),
                                    None,
                                    Box::new(Expr::Closure(
                                        a1.iter().map(|x| (x.clone(), None)).collect::<Vec<_>>(),
                                        None,
                                        Box::new(Expr::Call(
                                            Box::new(Expr::Ident(vec!["@g".to_owned()])),
                                            {
                                                let mut v = vec![Expr::Call(
                                                    Box::new(Expr::Ident(vec!["@f".to_owned()])),
                                                    a2.iter()
                                                        .map(|x| Expr::Ident(vec![x.to_owned()]))
                                                        .collect::<Vec<_>>(),
                                                )];
                                                v.append(
                                                    &mut a1[1..]
                                                        .iter()
                                                        .map(|x| Expr::Ident(vec![x.to_owned()]))
                                                        .collect::<Vec<_>>(),
                                                );
                                                v
                                            },
                                        )),
                                    )),
                                )),
                                vec![Expr::Literal(Literal::Null)],
                            );
                            eval_expr(&mut expr, new_ctx)
                        }
                    }
                    nonf => eval_expr(&mut Expr::Call(b.clone(), vec![*a.clone()]), context),
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
                            unsafe { &mut *context.as_ref().get() }.free_var.insert(
                                aa.last().unwrap().clone(),
                                Rc::new(UnsafeCell::new(unsafe { &*bbb.as_ref().get() }.clone())),
                            );
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
