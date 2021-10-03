mod elab;
use core::cell::UnsafeCell;

use alloc::{boxed::Box, collections::BTreeMap, rc::Rc, string::String, vec, vec::Vec};

use crate::{
    interpreter::type_checker::elab::type_elab,
    parser::{
        expr::{CommentedExpr, Expr},
        ty::Type,
        TopLevel,
    },
};

#[derive(Debug)]
pub struct TypeCheckContext {
    pub(crate) parent: Option<Rc<UnsafeCell<TypeCheckContext>>>,
    pub(crate) free_var: BTreeMap<String, Type>,
    pub(crate) alias: BTreeMap<String, Type>,
}
impl TypeCheckContext {
    fn get_alias<'a>(&'a mut self, alias: &'a String) -> Option<Type> {
        match self.alias.get_mut(alias) {
            Some(t) => Some(t.clone()),
            None => match &self.parent {
                Some(p) => unsafe { &mut *p.get() }.get_alias(alias).clone(),
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
            free_var: BTreeMap::new(),
            alias: BTreeMap::new(),
        }
    }
}

pub fn type_check_toplevel(
    toplevel: &mut TopLevel,
    context: Rc<UnsafeCell<TypeCheckContext>>,
) -> Result<Option<Type>, &'static str> {
    match toplevel {
        TopLevel::Comment(_) => Ok(None),
        TopLevel::TypeDef(_, _) => todo!(),
        TopLevel::EnumDef(_, _) => todo!(),
        TopLevel::Import(_, _) => todo!(),
        TopLevel::Expr(e) => Ok(type_check_expr(&mut *e, context).ok()),
    }
}

pub fn type_check_expr(
    e: &mut CommentedExpr,
    context: Rc<UnsafeCell<TypeCheckContext>>,
) -> Result<Type, &'static str> {
    let CommentedExpr { comment, expr: e } = e;
    match e {
        Expr::Quoted(e) => type_check_expr(e, context),
        Expr::Block(es) => {
            let ct = Rc::new(UnsafeCell::new(TypeCheckContext::new_with(context.clone())));
            let tys = es
                .iter_mut()
                .map(|e| type_check_toplevel(e, ct.clone()))
                .collect::<Vec<_>>();
            for i in (tys.len() - 1)..0 {
                if let Ok(Some(ty)) = tys[i].clone() {
                    return Ok(ty);
                }
            }
            Err("block element has no expr")
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
            for ((_, k), v) in es {
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

            Ok(Type::Function(
                aa,
                Box::new(type_elab(context, bb, b.clone().unwrap_or(Type::Any))?),
            ))
        }
        Expr::If(a, b, c) => {
            let mut aa = type_check_expr(a, context.clone())?;
            let mut bb = type_check_expr(b, context.clone())?;
            let mut cc = type_check_expr(c, context.clone())?;
            let _ = type_elab(context.clone(), aa, Type::Bool)?;
            type_elab(context, bb, cc)
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
                            bbbs[i] = type_elab(context.clone(), a[i].clone(), bbbs[i].clone())?;
                        }
                        let args = a[bbbs.len()..]
                            .iter()
                            .map(|t| t.clone())
                            .collect::<Vec<_>>();
                        Ok(Type::Function(args, b))
                    } else if a.len() < bbbs.len() {
                        if let box f @ Type::Function(_, _) = b {
                            let mut expr = CommentedExpr::from_expr(Expr::Call(
                                Box::new(CommentedExpr::from_expr(Expr::Call(
                                    func.clone(),
                                    args[0..a.len()]
                                        .iter()
                                        .map(|x| x.clone())
                                        .collect::<Vec<_>>(),
                                ))),
                                args[a.len()..]
                                    .iter()
                                    .map(|x| x.clone())
                                    .collect::<Vec<_>>(),
                            ));
                            type_check_expr(&mut expr, context)
                        } else {
                            Err("too much argument")
                        }
                    } else {
                        for i in 0..a.len() {
                            a[i] = type_elab(
                                context.clone(),
                                a[i].clone(),
                                type_check_expr(&mut args[i].clone(), context.clone())?,
                            )?;
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
                            type_elab(context, *bb, a[0].clone())
                        }
                    }
                    mut nonf => {
                        if a.len() < 1 {
                            Err("no enought args")
                        } else {
                            type_elab(context, a[0].clone(), nonf)
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
            let r = type_elab(context.clone(), aaa, bbb.clone())?;
            if let box CommentedExpr {
                comment,
                expr: Expr::Ident(aa),
            } = a
            {
                if aa.len() > 1 {
                    unimplemented!()
                } else {
                    match unsafe { &mut *context.get() }.get_free_var(aa.get(0).unwrap()) {
                        Some(fv) => {
                            unsafe { &mut *context.get() }
                                .free_var
                                .insert(aa[0].clone(), r.clone());
                        }
                        None => {
                            unsafe { &mut *context.get() }
                                .free_var
                                .insert(aa[0].clone(), r.clone());
                        }
                    }
                }
            }
            Ok(r)
        }
        Expr::SpecifyTyped(ex, ty) => {
            let mut tyy = type_check_expr(ex, context.clone())?;
            type_elab(context, tyy, ty.clone())
        }
    }
}
