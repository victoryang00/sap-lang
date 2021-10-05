use alloc::{
    borrow::ToOwned, boxed::Box, collections::BTreeMap, format, rc::Rc, string::String, vec,
    vec::Vec,
};
use core::{
    borrow::{Borrow, BorrowMut},
    cell::UnsafeCell,
    ops::{Deref, DerefMut},
};

use nom::error::context;

use crate::parser::{
    expr::{
        literal::{Literal, Number},
        reverse_bind, CommentedExpr, Expr,
    },
    TopLevel,
};

#[derive(Debug, Clone)]
pub struct EvalContext {
    pub(crate) parent: Option<Rc<UnsafeCell<EvalContext>>>,
    pub(crate) free_var: BTreeMap<String, Rc<UnsafeCell<Value>>>,
}
impl EvalContext {
    fn new_with(context: Rc<UnsafeCell<EvalContext>>) -> Self {
        Self {
            parent: Some(context),
            free_var: BTreeMap::new(),
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
impl core::fmt::Debug for Value {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Pair(s, o) => f
                .debug_tuple("Pair")
                .field(s)
                .field(unsafe { &*o.as_ref().get() })
                .finish(),
            Self::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::Function(arg0, arg1, arg2, arg3) => {
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

pub unsafe fn call_func_with_expr(
    f: Value,
    args: Vec<CommentedExpr>,
    context: Rc<UnsafeCell<EvalContext>>,
) -> Result<Rc<UnsafeCell<Value>>, &'static str> {
    let mut v = vec![];
    for a in &args {
        let a = Rc::new(UnsafeCell::new(
            unsafe { &*eval_expr(a, context.clone())?.get() }.clone(),
        ));
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
            let mut expr = CommentedExpr::from_expr(Expr::Closure(
                fargs[v.len()..]
                    .iter()
                    .map(|x| (x.clone(), None))
                    .collect::<Vec<_>>(),
                None,
                Box::new(CommentedExpr::from_expr(Expr::Call(
                    Box::new(CommentedExpr::from_expr(Expr::Ident(vec![
                        "@func".to_owned()
                    ]))),
                    {
                        let mut vv = args;
                        vv.append(
                            &mut fargs[v.len()..]
                                .iter()
                                .map(|x| CommentedExpr::from_expr(Expr::Ident(vec![x.clone()])))
                                .collect::<Vec<_>>(),
                        );
                        vv
                    },
                ))),
            ));
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
            let mut expr = CommentedExpr::from_expr(Expr::Closure(
                fargs[v.len()..]
                    .iter()
                    .map(|x| (x.clone(), None))
                    .collect::<Vec<_>>(),
                None,
                Box::new(CommentedExpr::from_expr(Expr::Call(
                    Box::new(CommentedExpr::from_expr(Expr::Ident(vec![
                        "@func".to_owned()
                    ]))),
                    {
                        let mut vv = args;
                        vv.append(
                            &mut fargs[v.len()..]
                                .iter()
                                .map(|x| CommentedExpr::from_expr(Expr::Ident(vec![x.clone()])))
                                .collect::<Vec<_>>(),
                        );
                        vv
                    },
                ))),
            ));
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

pub fn eval_toplevel(
    t: &TopLevel,
    context: Rc<UnsafeCell<EvalContext>>,
) -> Result<Rc<UnsafeCell<Value>>, &'static str> {
    match t {
        TopLevel::Comment(c) => Ok(Rc::new(UnsafeCell::new(Value::Null))),
        TopLevel::TypeDef(name, expr) => {
            let clos = eval_expr(expr, context.clone())?;
            unsafe { &mut *context.clone().get() }
                .free_var
                .insert(name.clone(), clos.clone());
            Ok(Rc::new(UnsafeCell::new(Value::Null)))
        }
        TopLevel::EnumDef(_, _) => todo!(),
        TopLevel::Import(_, _) => todo!(),
        TopLevel::Expr(box e) => eval_expr(e, context),
    }
}

pub fn eval_expr(
    e: &CommentedExpr,
    context: Rc<UnsafeCell<EvalContext>>,
) -> Result<Rc<UnsafeCell<Value>>, &'static str> {
    // println!("trace: expr: {:?} context: {:?}", e, unsafe {
    //     &*context.as_ref().get()
    // });
    let CommentedExpr { comment, expr: e } = e;
    match e {
        Expr::Pair(s, o) => Ok(Rc::new(UnsafeCell::new(Value::Pair(
            s.clone(),
            eval_expr(&*o, context)?,
        )))),
        Expr::Quoted(e) => eval_expr(e, context),
        Expr::Block(es) => {
            let ct = Rc::new(UnsafeCell::new(EvalContext::new_with(context.clone())));
            let mut r = Rc::new(UnsafeCell::new(Value::Null));
            for e in es {
                r = eval_toplevel(e, ct.clone())?;
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
            for ((_, k), v) in es {
                b.insert(k.clone(), eval_expr(v, context.clone()).unwrap());
            }
            Ok(Rc::new(UnsafeCell::new(Value::Object(b))))
        }
        Expr::Closure(a, b, c) => {
            let ct = Rc::new(UnsafeCell::new(EvalContext::new_with(context.clone())));
            Ok(Rc::new(UnsafeCell::new(Value::Function(
                a.iter().map(|(s, _)| s.clone()).collect::<Vec<_>>(),
                BTreeMap::new(),
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
        Expr::ErrorHandle(e) => match eval_expr(e.borrow(), context) {
            Ok(e) => {
                if let Value::Error(_) = unsafe { &*e.as_ref().get() } {
                    Ok(Rc::new(UnsafeCell::new(Value::Null)))
                } else {
                    Ok(e)
                }
            }
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
                            let mut expr = CommentedExpr::from_expr(Expr::Call(
                                Box::new(CommentedExpr::from_expr(Expr::Closure(
                                    a2.iter().map(|x| (x.clone(), None)).collect::<Vec<_>>(),
                                    None,
                                    Box::new(CommentedExpr::from_expr(Expr::Closure(
                                        a1.iter().map(|x| (x.clone(), None)).collect::<Vec<_>>(),
                                        None,
                                        Box::new(CommentedExpr::from_expr(Expr::Call(
                                            Box::new(CommentedExpr::from_expr(Expr::Ident(vec![
                                                "@g".to_owned(),
                                            ]))),
                                            {
                                                let mut v =
                                                    vec![CommentedExpr::from_expr(Expr::Call(
                                                        Box::new(CommentedExpr::from_expr(
                                                            Expr::Ident(vec!["@f".to_owned()]),
                                                        )),
                                                        a2.iter()
                                                            .map(|x| {
                                                                CommentedExpr::from_expr(
                                                                    Expr::Ident(vec![x.to_owned()]),
                                                                )
                                                            })
                                                            .collect::<Vec<_>>(),
                                                    ))];
                                                v.append(
                                                    &mut a1[1..]
                                                        .iter()
                                                        .map(|x| {
                                                            CommentedExpr::from_expr(Expr::Ident(
                                                                vec![x.to_owned()],
                                                            ))
                                                        })
                                                        .collect::<Vec<_>>(),
                                                );
                                                v
                                            },
                                        ))),
                                    ))),
                                ))),
                                vec![CommentedExpr::from_expr(Expr::Literal(Literal::Null))],
                            ));
                            eval_expr(&mut expr, new_ctx)
                        }
                    }
                    nonf => eval_expr(
                        &mut CommentedExpr::from_expr(Expr::Call(b.clone(), vec![*a.clone()])),
                        context,
                    ),
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
                    match a.get(n) {
                        Some(v) => Ok(v.clone()),
                        None => {
                            for _ in a.len()..=n {
                                a.push(Rc::new(UnsafeCell::new(Value::Null)))
                            }
                            Ok(a[n].clone())
                        }
                    }
                } else {
                    panic!("TYPE CHECKER?!")
                }
            } else if let Value::Pair(s, o) = aaa {
                if let Value::Number(n) = bbb {
                    match n {
                        Number::Integer(n) => {
                            if *n == 0 {
                                Ok(Rc::new(UnsafeCell::new(Value::String(s.clone()))))
                            } else if *n == 1 {
                                Ok(o.clone())
                            } else {
                                Err("Index only 0,1 allowed to index pair")
                            }
                        }
                        Number::Floating(n) => {
                            if *n == 0.0 {
                                Ok(Rc::new(UnsafeCell::new(Value::String(s.clone()))))
                            } else if *n == 1.0 {
                                Ok(o.clone())
                            } else {
                                Err("Index only 0,1 allowed to index pair")
                            }
                        }
                    }
                } else {
                    Err("only could index pair with number")
                }
            } else if let Value::Object(o) = aaa {
                if let Value::String(s) = bbb {
                    match o.get(s) {
                        Some(v) => Ok(v.clone()),
                        None => {
                            let v = Rc::new(UnsafeCell::new(Value::Null));
                            o.insert(s.clone(), v.clone());
                            Ok(v)
                        }
                    }
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
                    if let box CommentedExpr {
                        comment,
                        expr: Expr::Ident(aa),
                    } = a
                    {
                        if aa.len() > 1 {
                            unimplemented!()
                        } else {
                            /*
                            a = <a>;
                            ---
                            _b = (a)<a>;
                            a = _b(_b);
                            */
                            let name = aa.last().unwrap().clone();
                            let clos = Value::Function(
                                vec![name.clone()],
                                BTreeMap::new(),
                                context.clone(),
                                *b.clone(),
                            );
                            unsafe { &mut *context.as_ref().get() }
                                .free_var
                                .insert(name.clone(), Rc::new(UnsafeCell::new(clos.clone())));
                            let call_res = unsafe {
                                call_func_with_expr(
                                    clos,
                                    vec![CommentedExpr::from_expr(Expr::Ident(vec![name.clone()]))],
                                    context.clone(),
                                )
                            }?;
                            let call_res =
                                Rc::new(UnsafeCell::new(unsafe { &*call_res.get() }.clone()));
                            unsafe { &mut *context.as_ref().get() }
                                .free_var
                                .insert(name.clone(), call_res.clone());
                            Ok(call_res)
                        }
                    } else {
                        panic!("FUCK ME")
                    }
                }
            };
            aaa
        }
        Expr::SpecifyTyped(ex, ty) => eval_expr(ex.borrow(), context),
    }
}
