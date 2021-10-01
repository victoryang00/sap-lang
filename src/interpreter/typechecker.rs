use crate::parser::expr::Expr;
use crate::parser::TopLevel;
use crate::parser::{expr::literal::Number, ty::Type};
use std::borrow::{Borrow, BorrowMut};
use std::cell::UnsafeCell;
use std::collections::{BTreeMap, HashMap};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

#[derive(Debug)]
pub struct TypeCheckContext {
    pub(crate) parent: Option<Rc<UnsafeCell<TypeCheckContext>>>,
    pub(crate) free_var: HashMap<String, Type>,
    pub(crate) alias: HashMap<String, Type>,
}
impl TypeCheckContext {
    fn get_alias<'a>(&'a mut self, alias: &'a String) -> Option<*mut Type> {
        match self.alias.get_mut(alias) {
            Some(t) => Some(t),
            None => match &self.parent {
                Some(p) => unsafe { &mut *p.get() }.get_alias(alias),
                None => None,
            },
        }
    }
    fn get_free_var<'a>(&'a mut self, fv: &'a String) -> Option<*mut Type> {
        match self.free_var.get_mut(fv) {
            Some(t) => Some(t),
            None => match &self.parent {
                Some(p) => unsafe { &mut *p.get() }.get_free_var(fv),
                None => None,
            },
        }
    }
    fn new_with(context: Rc<UnsafeCell<TypeCheckContext>>) -> Self {
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
    context: Rc<UnsafeCell<TypeCheckContext>>,
) -> Result<Type, &'static str> {
    match e {
        Expr::Quoted(e) => type_check_expr(e, context),
        Expr::Block(es) => {
            let ct = Rc::new(UnsafeCell::new(TypeCheckContext::new_with(context.clone())));
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
                match unsafe { &mut *context.get() }.get_free_var(i.get(0).unwrap()) {
                    Some(fv) => Ok(unsafe { &mut *fv }.clone()),
                    None => Ok(Type::Any),
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
            let ct = Rc::new(UnsafeCell::new(TypeCheckContext::new_with(context.clone())));
            let mut aa = vec![];
            for (k, v) in a {
                aa.push(v.clone().unwrap_or(Type::Any));
                unsafe { &mut *ct.get() }
                    .free_var
                    .insert(k.clone(), v.clone().unwrap_or(Type::Any));
            }

            let mut bb = type_check_expr(c, ct)?;
            let r = match unsafe {
                type_elab(
                    context.as_ref().get(),
                    &mut bb,
                    b.as_mut().unwrap_or(&mut Type::Any),
                )
            } {
                Ok(_) => Ok(Type::Function(aa, Box::new(bb))),
                Err(_) => {
                    println!("{:?} {:?}", bb, b);
                    Err("Type not matched")
                }
            };
            r
        }
        Expr::If(a, b, c) => {
            let mut aa = type_check_expr(a, context.clone())?;
            let mut bb = type_check_expr(b, context.clone())?;
            let mut cc = type_check_expr(c, context.clone())?;
            let context = context.as_ref().get();
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
        Expr::Call(func, args) => {
            let aaa = type_check_expr(func, context.clone())?;
            let mut bbbs = args
                .iter_mut()
                .map(|b| type_check_expr(b, context.clone()).unwrap_or(Type::Any))
                .collect::<Vec<_>>();
            match aaa {
                Type::Any => Ok(Type::Function(bbbs, Box::new(Type::Any))),
                Type::Function(mut a, b) => {
                    if a.len() > bbbs.len() {
                        for i in 0..bbbs.len() {
                            match unsafe {
                                type_elab(
                                    context.as_ref().get(),
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
                        if let box f @ Type::Function(_, _) = b {
                            let mut expr = Expr::Call(
                                Box::new(Expr::Call(
                                    func.clone(),
                                    args[0..a.len()]
                                        .iter()
                                        .map(|x| x.clone())
                                        .collect::<Vec<_>>(),
                                )),
                                args[a.len()..]
                                    .iter()
                                    .map(|x| x.clone())
                                    .collect::<Vec<_>>(),
                            );
                            type_check_expr(&mut expr, context)
                        } else {
                            Err("too much argument")
                        }
                    } else {
                        for i in 0..a.len() {
                            match unsafe {
                                type_elab(
                                    context.as_ref().get(),
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
            let mut bbb = type_check_expr(b, context.clone())?;
            match bbb {
                Type::Any => Ok(Type::Any),
                Type::Function(mut a, b) => match aaa {
                    Type::Function(aa, mut bb) => {
                        if a.len() < 1 {
                            Err("no enought args")
                        } else {
                            let mut aa = aa;
                            for i in 1..a.len() {
                                aa.push(a[i].clone());
                            }
                            let r = match unsafe {
                                type_elab(
                                    context.as_ref().get(),
                                    bb.as_mut(),
                                    a.get_mut(0).unwrap(),
                                )
                            } {
                                Ok(_) => Ok(Type::Function(aa, b)),
                                Err(_) => Err("Type not matched"),
                            };
                            r
                        }
                    }
                    mut nonf => {
                        if a.len() < 1 {
                            Err("no enought args")
                        } else {
                            let r = match unsafe {
                                type_elab(context.as_ref().get(), a.get_mut(0).unwrap(), &mut nonf)
                            } {
                                Ok(_) => {
                                    let a = a[1..].iter().map(|x| x.clone()).collect::<Vec<_>>();
                                    if a.len() == 0 {
                                        Ok(*b)
                                    } else {
                                        Ok(Type::Function(a, b))
                                    }
                                }
                                Err(_) => Err("Type not matched"),
                            };
                            r
                        }
                    }
                },
                Type::Alias(a) => {
                    todo!()
                }
                _ => Err("not bindable"),
            }
        }
        // TODO: elab index
        Expr::Index(a, b) => Ok(Type::Any),
        Expr::Assign(a, b) => {
            let mut aaa = type_check_expr(a, context.clone())?;
            let mut bbb = type_check_expr(b, context.clone())?;
            let r = match unsafe { type_elab(context.as_ref().get(), &mut aaa, &mut bbb) } {
                Ok(_) => Ok(bbb.clone()),
                Err(_) => Err("Type not matched"),
            };
            if let box Expr::Ident(aa) = a {
                if aa.len() > 1 {
                    unimplemented!()
                } else {
                    match unsafe { &mut *context.get() }.get_free_var(aa.get(0).unwrap()) {
                        Some(fv) => {}
                        None => {
                            unsafe { &mut *context.get() }
                                .free_var
                                .insert(aa[0].clone(), bbb);
                        }
                    }
                }
            }
            r
        }
        Expr::SpecifyTyped(ex, ty) => {
            let mut tyy = type_check_expr(ex, context.clone())?;
            match unsafe { type_elab(context.as_ref().get(), &mut tyy, ty) } {
                Ok(_) => Ok(ty.clone()),
                Err(_) => Err("Type not matched"),
            }
        }
    }
}
