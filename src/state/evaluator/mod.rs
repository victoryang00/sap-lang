use std::{borrow::Borrow, cell::UnsafeCell};
use std::{
    borrow::ToOwned, boxed::Box, collections::BTreeMap, rc::Rc, string::String, vec, vec::Vec,
};

use crate::parser::expr::{
    literal::{Literal, Number},
    CommentedExpr, Expr,
};
use crate::parser::top_level::TopLevel;
use crate::state::evaluator::value::Value;

pub mod value;
pub struct EvaledImports {
    pub imports: BTreeMap<String, Rc<UnsafeCell<EvalContext>>>,
}

#[derive(Debug, Clone)]
pub struct EvalContext {
    pub imports: Rc<UnsafeCell<EvaledImports>>,
    pub current_scope_import_alias: BTreeMap<String, String>,
    pub(crate) eval_parent_scope: Option<Rc<UnsafeCell<EvalContext>>>,
    pub(crate) eval_current_scope_variables: BTreeMap<String, Rc<UnsafeCell<Value>>>,
}
impl EvalContext {
    fn new_eval_sup_scope(context: Rc<UnsafeCell<EvalContext>>) -> Self {
        Self {
            imports: unsafe { &*context.get() }.imports.clone(),
            current_scope_import_alias: BTreeMap::new(),
            eval_parent_scope: Some(context),
            eval_current_scope_variables: BTreeMap::new(),
        }
    }

    fn get_eval_variable_by_name(&self, k: &[String]) -> Option<Rc<UnsafeCell<Value>>> {
        if k.len() == 1 {
            if self
                .eval_current_scope_variables
                .contains_key(k.last().unwrap())
            {
                Some(self.eval_current_scope_variables[k.last().unwrap()].clone())
            } else {
                if let Some(p) = self.eval_parent_scope.clone() {
                    (unsafe { &*p.get() }).get_eval_variable_by_name(k)
                } else {
                    None
                }
            }
        } else {
            get_source(Rc::new(UnsafeCell::new(self.clone())), k)
        }
    }
}

fn get_source(context: Rc<UnsafeCell<EvalContext>>, s: &[String]) -> Option<Rc<UnsafeCell<Value>>> {
    let c = unsafe { &*context.get() };
    match c.current_scope_import_alias.get(&s[0]) {
        Some(source) => unsafe { &*unsafe { &*c.imports.get() }.imports[&s[0]].get() }
            .get_eval_variable_by_name(&s[1..]),
        None => get_source(c.eval_parent_scope.as_ref()?.clone(), s),
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
                (&mut *ct.get())
                    .eval_current_scope_variables
                    .insert(k.clone(), v.clone());
            }
            let r = eval_expr(fb, ct.clone())?;
            let r = r.as_ref();
            let r = unsafe { &mut *r.get() };
            call_func_with_expr(
                r.clone(),
                args[h.len()..]
                    .iter()
                    .map(|x| x.clone())
                    .collect::<Vec<_>>(),
                context,
            )
        } else if v.len() < fargs.len() {
            let mut ctx = EvalContext::new_eval_sup_scope(context);
            ctx.eval_current_scope_variables
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
                (&mut *ct.get())
                    .eval_current_scope_variables
                    .insert(k.clone(), v.clone());
            }
            let r = eval_expr(fb, ct.clone());
            *h = hh;
            r
        }
    } else if let Value::NativeFunction(_name, fargs, mut h, ff) = f.clone() {
        if v.len() > fargs.len() {
            let _hh = h.clone();
            let mut vv = vec![];
            for i in 0..fargs.len() {
                h.insert(fargs[h.len()].clone(), v[i].clone());
            }
            for i in fargs {
                vv.push(h.get(&i).unwrap().clone().as_ref().get())
            }
            let (p, len, _) = vv.into_raw_parts();
            let r = Rc::new(ff(len, p));
            let r = r.as_ref();
            let r = unsafe { &mut *r.get() };
            call_func_with_expr(
                r.clone(),
                args[h.len()..]
                    .iter()
                    .map(|x| x.clone())
                    .collect::<Vec<_>>(),
                context,
            )
        } else if v.len() < fargs.len() {
            let mut ctx = EvalContext::new_eval_sup_scope(context);
            ctx.eval_current_scope_variables
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
            let _hh = h.clone();
            let mut vv = vec![];
            for i in 0..fargs.len() {
                h.insert(fargs[h.len()].clone(), v[i].clone());
            }
            for i in fargs {
                vv.push(h.get(&i).unwrap().clone().as_ref().get())
            }
            let (p, len, _) = vv.into_raw_parts();
            let r = Rc::new(ff(len, p));
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
        TopLevel::Comment(_c) => Ok(Rc::new(UnsafeCell::new(Value::Null))),
        TopLevel::TypeDef(name, expr) => {
            let clos = eval_expr(expr, context.clone())?;
            unsafe { &mut *context.clone().get() }
                .eval_current_scope_variables
                .insert(name.clone(), clos.clone());
            Ok(Rc::new(UnsafeCell::new(Value::Null)))
        }
        TopLevel::EnumDef(_, _) => todo!(),
        TopLevel::ImportPath(_, _) => todo!(),
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
    let CommentedExpr {
        comment: _,
        expr: e,
    } = e;
    match e {
        Expr::Pair(s, o) => Ok(Rc::new(UnsafeCell::new(Value::Pair(
            s.clone(),
            eval_expr(&*o, context)?,
        )))),
        Expr::Quoted(e) => eval_expr(e, context),
        Expr::Block(es) => {
            let ct = Rc::new(UnsafeCell::new(EvalContext::new_eval_sup_scope(
                context.clone(),
            )));
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
            match unsafe { &*context.get() }.get_eval_variable_by_name(i.as_slice()) {
                Some(n) => Ok(n),
                None => Err("no var"),
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
        Expr::Closure(a, _b, c) => {
            let ct = Rc::new(UnsafeCell::new(EvalContext::new_eval_sup_scope(
                context.clone(),
            )));
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
            let aaa = unsafe { &mut *aaa.get() };
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
        Expr::MultiIf(_a, _b) => todo!(),
        Expr::For(_a, _b, _c) => {
            todo!();
            Ok(Rc::new(UnsafeCell::new(Value::Null)))
        }
        Expr::Call(func, args) => {
            let f = eval_expr(func, context.clone())?;
            let f = f.as_ref();
            let f = unsafe { &mut *f.get() };
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
            let _raaa = aaa.as_ref();
            let raaa = unsafe { &mut *aaa.get() };

            let bbb = eval_expr(b, context.clone())?;
            let _rbbb = bbb.as_ref();
            let rbbb = unsafe { &mut *bbb.get() };
            match rbbb {
                Value::Function(a1, _hs, _ct, _e) => match raaa.clone() {
                    Value::Function(a2, _bb, _cct, _ee) => {
                        let new_ctx =
                            Rc::new(UnsafeCell::new(EvalContext::new_eval_sup_scope(context)));
                        unsafe {
                            { &mut *new_ctx.as_ref().get() }
                                .eval_current_scope_variables
                                .insert("@f".to_owned(), aaa);
                            { &mut *new_ctx.as_ref().get() }
                                .eval_current_scope_variables
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
                    _nonf => eval_expr(
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
            let aaa = unsafe { &mut *aaa.get() };

            let bbb = eval_expr(b, context.clone())?;
            let bbb = bbb.as_ref();
            let bbb = unsafe { &mut *bbb.get() };
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
            let aaa = match eval_expr(a, context.clone()) {
                Ok(aaa) => {
                    let bbb = eval_expr(b, context.clone())?;
                    *unsafe { &mut *aaa.get() } = unsafe { &mut *bbb.get() }.clone();
                    Ok(bbb)
                }
                Err(_) => {
                    let _bbb = eval_expr(b, context.clone())?;
                    if let box CommentedExpr {
                        comment: _,
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
                                .eval_current_scope_variables
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
                                .eval_current_scope_variables
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
        Expr::SpecifyTyped(ex, _ty) => eval_expr(ex.borrow(), context),
    }
}
