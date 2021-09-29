use crate::parser::expr::Expr;
use crate::parser::TopLevel;
use crate::parser::{expr::literal::Number, ty::Type};
use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
// struct Context {
//     variables: HashMap<String, Box<Value>>,
// }
// impl Context {
//     fn new() -> Self {
//         Self {
//             variables: HashMap::new(),
//         }
//     }
//     // assign to an ident
//     fn assign(&mut self, name: String, value: Box<Value>) {
//         self.variables.insert(name, value);
//     }
//     // assign to an expr
//     fn assign_right(&mut self, mut value: Box<Value>, new_value: Value) {
//         *value = new_value;
//     }
//     // fn get_variable(&self, name: String) -> Option<Box<Value>> {
//     //     self.variables.get(&name).and_then(|x| x.clone())
//     // }
// }

// enum Value {
//     Any(Box<Value>),
//     // AnyArgsList,
//     Number(Number),
//     String(String),
//     Bool(bool),
//     Function(Option<Box<Context>>, Context, Expr),
//     Array(Vec<Value>),
//     Object(BTreeMap<String, Value>),
//     Enum(BTreeMap<String, Value>),
//     /// could not return a null without specify the type
//     Null,
//     /// same as null
//     Error(String),
// }

// // return last expr
// fn eval(top_levels: Vec<TopLevel>) -> Value {
//     let mut context = Context::new();
//     let mut res = Value::Null;
//     for t in top_levels {
//         eval_top_level(t, &mut context);
//     }
//     res
// }

// fn eval_top_level(t: TopLevel, c: &mut Context) {}

// fn eval_expr(e: Expr, c: &mut Context) {}

#[derive(Debug)]
pub struct TypeCheckContext {
    pub(crate) parent: Option<Rc<RefCell<TypeCheckContext>>>,
    pub(crate) free_var: HashMap<String, Type>,
    pub(crate) alias: HashMap<String, Type>,
}
impl TypeCheckContext {
    fn get_alias<'a>(&'a mut self, alias: &'a String) -> Option<*mut Type> {
        match self.alias.get_mut(alias) {
            Some(t) => Some(t),
            None => match &self.parent {
                Some(p) => p.as_ref().borrow_mut().get_alias(alias),
                None => None,
            },
        }
    }
    fn new_with(context: Rc<RefCell<TypeCheckContext>>) -> Self {
        Self {
            parent: Some(context),
            free_var: HashMap::new(),
            alias: HashMap::new(),
        }
    }
}

/// TODO: trace memory leak
unsafe fn type_elab(context: *mut TypeCheckContext, a: *mut Type, b: *mut Type) -> Result<(), ()> {
    let context = &mut *context;
    match (&mut *a, &mut *b) {
        (Type::Any, _) => {
            // drop_in_place(a);
            *a = (&*b).clone();
        }
        // number
        (Type::Number, Type::Any) => {
            // drop_in_place(b);
            *b = (&*a).clone();
        }
        (Type::Number, Type::Number) => {}
        (Type::Number, Type::Alias(alias)) => {
            return type_elab(context, a, context.get_alias(alias).unwrap());
        }
        (Type::Number, _) => {
            return Err(());
        }
        // string
        (Type::String, Type::Any) => {
            // drop_in_place(b);
            *b = (&*a).clone();
        }
        (Type::String, Type::Alias(alias)) => {
            return type_elab(context, a, context.get_alias(alias).unwrap());
        }
        (Type::String, Type::String) => {}
        (Type::String, _) => {
            return Err(());
        }
        // bool
        (Type::Bool, Type::Any) => {
            // drop_in_place(b);
            *b = (&*a).clone();
        }
        (Type::Bool, Type::Bool) => {}
        (Type::Bool, Type::Alias(alias)) => {
            return type_elab(context, a, context.get_alias(alias).unwrap());
        }
        (Type::Bool, _) => {
            return Err(());
        }
        // function
        (Type::Function(_, _), Type::Any) => {
            // drop_in_place(b);
            *b = (&*a).clone();
        }
        (Type::Function(args1, ret1), Type::Function(args2, ret2)) => {
            match args1.len().cmp(&args2.len()) {
                std::cmp::Ordering::Equal => {
                    if args1
                        .iter_mut()
                        .map(|t| {
                            match args2
                                .iter_mut()
                                .position(|r| type_elab(context, t, r).is_ok())
                            {
                                Some(p) => true,
                                None => false,
                            }
                        })
                        .fold(true, |a, b| a && b)
                    {
                        return type_elab(context, ret1.as_mut(), ret2.as_mut());
                    }
                }
                _ => {
                    return Err(());
                }
            }
        }
        (Type::Function(_, _), Type::Alias(alias)) => {
            return type_elab(context, a, context.get_alias(alias).unwrap());
        }
        (Type::Function(_, _), _) => {
            return Err(());
        }
        // array
        (Type::Array, Type::Any) => {
            // drop_in_place(b);
            *b = (&*a).clone();
        }
        (Type::Array, Type::Array) => {}
        (Type::Array, Type::Alias(alias)) => {
            return type_elab(context, a, context.get_alias(alias).unwrap());
        }
        (Type::Array, _) => {
            return Err(());
        }
        // object
        (Type::Object(_), Type::Any) => {
            // drop_in_place(b);
            *b = (&*a).clone();
        }
        (Type::Object(ass), Type::Object(bss)) => {
            let mut m = BTreeMap::new();
            for (k, v) in ass {
                m.insert(k.clone(), v.clone());
            }
            for (k, v) in bss {
                if m.contains_key(k) {
                    return type_elab(context, m.get_mut(k).unwrap(), v);
                } else {
                    m.insert(k.clone(), v.clone());
                }
            }
            // drop_in_place(a);
            // drop_in_place(b);
            *a = Type::Object(m.clone());
            *b = Type::Object(m);
        }
        (Type::Object(_), Type::Alias(alias)) => {
            return type_elab(context, a, context.get_alias(alias).unwrap());
        }
        (Type::Object(_), _) => {
            return Err(());
        }
        // enum
        (Type::Enum(_), Type::Any) => {
            // drop_in_place(b);
            *b = (&*a).clone();
        }
        (Type::Enum(_), Type::Enum(_)) => todo!(),
        (Type::Enum(_), Type::Alias(alias)) => {
            return type_elab(context, a, context.get_alias(alias).unwrap());
        }
        (Type::Enum(_), e) => todo!(),
        // alias
        (Type::Alias(_), Type::Any) => {
            // drop_in_place(b);
            *b = (&*a).clone();
        }
        (Type::Alias(alias), b) => {
            return type_elab(context, context.get_alias(alias).unwrap(), b);
        }
    };
    Ok(())
}

pub fn type_check_expr(
    e: &mut Expr,
    context: Rc<RefCell<TypeCheckContext>>,
) -> Result<Type, &'static str> {
    match e {
        Expr::Block(es) => {
            let ct = Rc::new(RefCell::new(TypeCheckContext::new_with(context.clone())));
            let tys = es
                .iter_mut()
                .map(|e| type_check_expr(e, ct.clone()))
                .collect::<Vec<_>>();
            tys.last().unwrap_or(&Ok(Type::Any)).clone()
        }
        Expr::Literal(l) => match l {
            crate::parser::expr::literal::Literal::Null => Ok(Type::Any),
            crate::parser::expr::literal::Literal::Bool(_) => Ok(Type::Bool),
            crate::parser::expr::literal::Literal::String(_) => Ok(Type::String),
            crate::parser::expr::literal::Literal::Number(_) => Ok(Type::Number),
        },
        Expr::Ident(i) => {
            if i.len() > 1 {
                unimplemented!()
            } else {
                if !context
                    .deref()
                    .borrow_mut()
                    .free_var
                    .contains_key(i.last().unwrap())
                {
                    context
                        .deref()
                        .borrow_mut()
                        .free_var
                        .insert(i.last().unwrap().clone(), Type::Any);
                    Ok(Type::Any)
                } else {
                    Ok(context
                        .deref()
                        .borrow_mut()
                        .free_var
                        .get(i.last().unwrap())
                        .unwrap()
                        .clone())
                }
            }
        }
        Expr::Array(_) => Ok(Type::Array),
        Expr::Object(es) => {
            let mut b = BTreeMap::new();
            for (k, v) in es {
                b.insert(
                    k.clone(),
                    type_check_expr(v, context.clone()).unwrap_or(Type::Any),
                );
            }
            Ok(Type::Object(b))
        }
        Expr::Closure(a, b, c) => {
            let ct = Rc::new(RefCell::new(TypeCheckContext::new_with(context.clone())));
            let mut aa = vec![];
            for (k, v) in a {
                aa.push(v.clone().unwrap_or(Type::Any));
                ct.as_ref()
                    .borrow_mut()
                    .free_var
                    .insert(k.clone(), v.clone().unwrap_or(Type::Any));
            }

            let mut bb = type_check_expr(c, ct)?;
            let r = match unsafe {
                type_elab(
                    context.clone().deref().borrow_mut().deref_mut(),
                    &mut bb,
                    b.as_mut().unwrap_or(&mut Type::Any),
                )
            } {
                Ok(_) => Ok(Type::Function(aa, Box::new(bb))),
                Err(_) => Err("Type not matched"),
            };
            r
        }
        Expr::If(a, b, c) => {
            let mut aa = type_check_expr(a, context.clone())?;
            let mut bb = type_check_expr(b, context.clone())?;
            let mut cc = type_check_expr(c, context.clone())?;
            let context = context.clone().deref().borrow_mut().deref_mut() as *mut _;
            let r = match unsafe { type_elab(context, &mut aa, &mut Type::Bool) } {
                Ok(_) => {
                    let r = match unsafe { type_elab(context, &mut bb, &mut cc) } {
                        Ok(_) => Ok(bb),
                        Err(_) => Err("Type not matched"),
                    };
                    r
                }
                Err(_) => Err("Type not matched"),
            };
            r
        }
        Expr::MultiIf(a, b) => todo!(),
        Expr::For(_, _, _) => Ok(Type::Any),
        Expr::Call(a, b) => {
            let aaa = type_check_expr(a, context.clone())?;
            let mut bbbs = b
                .iter_mut()
                .map(|b| type_check_expr(b, context.clone()).unwrap_or(Type::Any))
                .collect::<Vec<_>>();
            match aaa {
                Type::Any => Ok(Type::Function(bbbs, Box::new(Type::Any))),
                // TODO: CURRY
                Type::Function(mut a, b) => {
                    if a.len() > bbbs.len() {
                        for i in 0..bbbs.len() {
                            match unsafe {
                                type_elab(
                                    context.clone().deref().borrow_mut().deref_mut(),
                                    a.get_mut(i).unwrap(),
                                    bbbs.get_mut(i).unwrap(),
                                )
                            } {
                                Ok(_) => {
                                    continue;
                                }
                                Err(_) => {
                                    return Err("type mismatch");
                                }
                            }
                        }
                        let args = a[bbbs.len()..]
                            .iter()
                            .map(|t| t.clone())
                            .collect::<Vec<_>>();
                        Ok(Type::Function(args, b))
                    } else if a.len() < bbbs.len() {
                        Err("argument count too big")
                    } else {
                        for i in 0..a.len() {
                            match unsafe {
                                type_elab(
                                    context.clone().deref().borrow_mut().deref_mut(),
                                    a.get_mut(i).unwrap(),
                                    bbbs.get_mut(i).unwrap(),
                                )
                            } {
                                Ok(_) => {
                                    continue;
                                }
                                Err(_) => {
                                    return Err("type mismatch");
                                }
                            }
                        }
                        Ok(*b)
                    }
                }
                _ => Err("call with other expr"),
            }
        }
        Expr::ErrorHandle(e) => type_check_expr(e, context),
        // TODO: FUCK ME
        Expr::Bind(a, b) => {
            let mut aaa = type_check_expr(a, context.clone())?;
            match aaa {
                Type::Object(o) => match b.clone() {
                    box Expr::Call(i, mut bb) => {
                        let mut c = vec![*a.clone()];
                        c.append(&mut bb);
                        bb = c;
                        type_check_expr(b, context)
                    }
                    box Expr::Ident(s) => Ok(o.get(s.last().unwrap()).unwrap().clone()),
                    _ => Err("WTF"),
                },
                Type::Function(aaa, mut bbb) => match b.clone() {
                    box Expr::Call(i, mut bb) => {
                        let mut c = vec![*a.clone()];
                        c.append(&mut bb);
                        bb = c;
                        type_check_expr(b, context)
                    }
                    box Expr::Ident(s) => {
                        if s.len() > 1 {
                            unimplemented!()
                        } else {
                            if context
                                .deref()
                                .borrow_mut()
                                .free_var
                                .contains_key(s.last().unwrap())
                            {
                                let ptr = context.deref().borrow_mut().deref_mut() as *mut _;
                                if let Some(Type::Function(_aaa, _bbb)) = context
                                    .deref()
                                    .borrow_mut()
                                    .free_var
                                    .get_mut(s.last().unwrap())
                                {
                                    let (_h, _t) = _aaa.split_at_mut(1);
                                    let r = unsafe { type_elab(ptr, &mut _h[0], bbb.as_mut()) }
                                        .unwrap();
                                    Ok(Type::Function(
                                        _t.iter().map(|s| s.clone()).collect::<Vec<_>>(),
                                        _bbb.clone(),
                                    ))
                                } else {
                                    Err("Is not Even A FUNCTION!")
                                }
                            } else {
                                Err("WTF")
                            }
                        }
                    }
                    box mut c @ Expr::Bind(_, _) => {
                        let inner = type_check_expr(&mut c, context)?;
                        todo!("not implemented yet")
                    }
                    _ => Err("Could not dot attr for function"),
                },
                _ => match b.clone() {
                    box Expr::Call(i, mut bb) => {
                        let mut c = vec![*a.clone()];
                        c.append(&mut bb);
                        bb = c;
                        type_check_expr(b, context)
                    }
                    _ => Err("Fuck ME"),
                },
            }
        }
        // TODO: elab index
        Expr::Index(a, b) => Ok(Type::Any),
        Expr::Assign(a, b) => {
            let mut aaa = type_check_expr(a, context.clone())?;
            let mut bbb = type_check_expr(b, context.clone())?;
            let r = match unsafe {
                type_elab(
                    context.clone().deref().borrow_mut().deref_mut(),
                    &mut aaa,
                    &mut bbb,
                )
            } {
                Ok(_) => Ok(bbb),
                Err(_) => Err("Type not matched"),
            };
            if let box Expr::Ident(aa) = a {
                if aa.len() > 1 {
                    unimplemented!()
                } else {
                    if context
                        .deref()
                        .borrow_mut()
                        .free_var
                        .contains_key(aa.last().unwrap())
                    {
                        context
                            .deref()
                            .borrow_mut()
                            .free_var
                            .insert(aa.last().unwrap().clone(), aaa.clone());
                    }
                }
            }
            r
        }
        Expr::SpecifyTyped(ex, ty) => {
            let mut tyy = type_check_expr(ex, context.clone())?;
            match unsafe {
                type_elab(
                    context.clone().deref().borrow_mut().deref_mut(),
                    &mut tyy,
                    ty,
                )
            } {
                Ok(_) => Ok(ty.clone()),
                Err(_) => Err("Type not matched"),
            }
        }
    }
}
