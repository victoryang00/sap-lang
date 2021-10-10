mod elab;

use std::cell::UnsafeCell;

use std::{boxed::Box, collections::BTreeMap, rc::Rc, string::String, vec, vec::Vec};

use crate::parser::top_level::TopLevel;
use crate::{
    parser::{
        expr::{CommentedExpr, Expr},
        ty::Type,
    },
    state::type_checker::elab::type_elab,
};

pub struct TypeCheckedImports {
    pub imports: BTreeMap<String, Rc<UnsafeCell<TypeCheckContext>>>,
}

#[derive(Debug, Clone)]
pub struct TypeCheckContext {
    pub imports: Rc<UnsafeCell<TypeCheckedImports>>,
    pub current_scope_import_alias: BTreeMap<String, String>,
    pub(crate) type_check_parent_scope: Option<Rc<UnsafeCell<TypeCheckContext>>>,
    pub(crate) type_check_current_scope_variables: BTreeMap<String, Type>,
    pub(crate) type_check_current_scope_type_alias: BTreeMap<String, Type>,
}

fn get_var_from_imports(context: Rc<UnsafeCell<TypeCheckContext>>, s: &[String]) -> Option<Type> {
    let c = unsafe { &*context.get() };
    match c.current_scope_import_alias.get(&s[0]) {
        Some(source) => unsafe { &*unsafe { &*c.imports.get() }.imports[&s[0]].get() }
            .get_variable_type_by_name(&s[1..]),
        None => get_var_from_imports(c.type_check_parent_scope.as_ref()?.clone(), s),
    }
}
fn get_alias_from_imports(context: Rc<UnsafeCell<TypeCheckContext>>, s: &[String]) -> Option<Type> {
    let c = unsafe { &*context.get() };
    match c.current_scope_import_alias.get(&s[0]) {
        Some(source) => unsafe { &*unsafe { &*c.imports.get() }.imports[&s[0]].get() }
            .get_variable_type_by_name(&s[1..]),
        None => get_var_from_imports(c.type_check_parent_scope.as_ref()?.clone(), s),
    }
}

impl TypeCheckContext {
    fn get_type_alias<'a>(&self, k: &'a [String]) -> Option<Type> {
        if k.len() == 1 {
            if self
                .type_check_current_scope_type_alias
                .contains_key(k.last().unwrap())
            {
                Some(self.type_check_current_scope_type_alias[k.last().unwrap()].clone())
            } else {
                if let Some(p) = self.type_check_parent_scope.clone() {
                    (unsafe { &*p.get() }).get_type_alias(k)
                } else {
                    None
                }
            }
        } else {
            get_alias_from_imports(Rc::new(UnsafeCell::new(self.clone())), k)
        }
    }

    fn get_variable_type_by_name(&self, k: &[String]) -> Option<Type> {
        if k.len() == 1 {
            if self
                .type_check_current_scope_variables
                .contains_key(k.last().unwrap())
            {
                Some(self.type_check_current_scope_variables[k.last().unwrap()].clone())
            } else {
                if let Some(p) = self.type_check_parent_scope.clone() {
                    (unsafe { &*p.get() }).get_variable_type_by_name(k)
                } else {
                    None
                }
            }
        } else {
            get_var_from_imports(Rc::new(UnsafeCell::new(self.clone())), k)
        }
    }
    fn new_type_checking_sub_scope(context: Rc<UnsafeCell<TypeCheckContext>>) -> Self {
        Self {
            imports: unsafe { &*context.get() }.imports.clone(),
            type_check_parent_scope: Some(context),
            type_check_current_scope_variables: BTreeMap::new(),
            type_check_current_scope_type_alias: BTreeMap::new(),

            current_scope_import_alias: BTreeMap::new(),
        }
    }
}

pub fn type_check_toplevel(
    toplevel: &TopLevel,
    context: Rc<UnsafeCell<TypeCheckContext>>,
) -> Result<Option<Type>, &'static str> {
    match toplevel {
        TopLevel::Comment(_) => Ok(None),
        TopLevel::TypeDef(s, e) => {
            let clos = type_check_expr(e, context.clone())?;
            if let Type::Function(args, r) = clos.clone() {
                unsafe { &mut *context.clone().get() }
                    .type_check_current_scope_variables
                    .insert(
                        s.clone(),
                        Type::Function(args, Box::new(Type::Alias(vec![s.clone()]))),
                    );
                unsafe { &mut *context.clone().get() }
                    .type_check_current_scope_type_alias
                    .insert(s.clone(), *r.clone());
                Ok(Some(Type::Alias(vec![s.clone()])))
            } else {
                Err("not f")
            }
        }
        TopLevel::EnumDef(_, _) => todo!(),
        TopLevel::ImportPath(_, _) => todo!(),
        TopLevel::Expr(e) => Ok(type_check_expr(&*e, context).ok()),
    }
}

pub fn type_check_expr(
    e: &CommentedExpr,
    context: Rc<UnsafeCell<TypeCheckContext>>,
) -> Result<Type, &'static str> {
    let CommentedExpr {
        comment: _,
        expr: e,
    } = e;
    match e {
        Expr::Pair(s, e) => Ok(Type::Pair(
            s.clone(),
            Box::new(type_check_expr(&*e, context)?),
        )),
        Expr::Quoted(e) => type_check_expr(e, context),
        Expr::Block(es) => {
            let ct = Rc::new(UnsafeCell::new(
                TypeCheckContext::new_type_checking_sub_scope(context.clone()),
            ));
            let mut tys = es
                .iter()
                .map(|e| type_check_toplevel(e, ct.clone()))
                .collect::<Vec<_>>();
            tys.reverse();
            for i in 0..tys.len() {
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
            match unsafe { &mut *context.get() }.get_variable_type_by_name(i.as_slice()) {
                Some(fv) => Ok(fv.clone()),
                None => Ok(Type::Any),
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
            let ct = Rc::new(UnsafeCell::new(
                TypeCheckContext::new_type_checking_sub_scope(context.clone()),
            ));
            let mut aa = vec![];
            for (k, v) in a {
                aa.push(v.clone().unwrap_or(Type::Any));
                unsafe { &mut *ct.get() }
                    .type_check_current_scope_variables
                    .insert(k.clone(), v.clone().unwrap_or(Type::Any));
            }

            let bb = type_check_expr(c, ct)?;

            Ok(Type::Function(
                aa,
                Box::new(type_elab(context, bb, b.clone().unwrap_or(Type::Any))?),
            ))
        }
        Expr::If(a, b, c) => {
            let aa = type_check_expr(a, context.clone())?;
            let bb = type_check_expr(b, context.clone())?;
            let cc = type_check_expr(c, context.clone())?;
            let _ = type_elab(context.clone(), aa, Type::Bool)?;
            type_elab(context, bb, cc)
        }
        Expr::MultiIf(_a, _b) => todo!(),
        Expr::For(_, _, _) => Ok(Type::Any),
        Expr::Call(func, args) => {
            let aaa = type_check_expr(func, context.clone())?;
            let mut bbbs = args
                .iter()
                .map(|b| type_check_expr(b, context.clone()).unwrap_or(Type::Any))
                .collect::<Vec<_>>();
            match aaa {
                Type::Any => Ok(Type::Any),
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
                        if let box _f @ Type::Function(_, _) = b {
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
        Expr::Bind(ba, bb) => {
            let aaa = type_check_expr(ba, context.clone())?;
            let bbb = type_check_expr(bb, context.clone())?;
            match bbb {
                Type::Any => Ok(Type::Any),
                Type::Function(a, _b) => match aaa {
                    Type::Function(aa, bb) => {
                        if a.len() < 1 {
                            type_check_expr(
                                &mut CommentedExpr::from_expr(Expr::Call(ba.clone(), vec![])),
                                context,
                            )
                        } else {
                            let mut aa = aa;
                            for i in 1..a.len() {
                                aa.push(a[i].clone());
                            }
                            type_elab(context, *bb, a[0].clone())
                        }
                    }
                    _nonf => type_check_expr(
                        &mut CommentedExpr::from_expr(Expr::Call(bb.clone(), vec![*ba.clone()])),
                        context,
                    ),
                },
                Type::Alias(_a) => {
                    todo!()
                }
                _ => Err("not bindable"),
            }
        }
        // TODO: elab index
        Expr::Index(_a, _b) => Ok(Type::Any),
        Expr::Assign(a, b) => {
            let aaa = type_check_expr(a, context.clone())?;

            if let box CommentedExpr {
                comment: _,
                expr: Expr::Ident(aa),
            } = a
            {
                match unsafe { &mut *context.get() }.get_variable_type_by_name(aa.as_slice()) {
                    Some(_fv) => {
                        unsafe { &mut *context.get() }
                            .type_check_current_scope_variables
                            .insert(aa[0].clone(), Type::Any);
                        let bbb = type_check_expr(b, context.clone())?;
                        let r = type_elab(context.clone(), aaa, bbb.clone())?;
                        unsafe { &mut *context.get() }
                            .type_check_current_scope_variables
                            .insert(aa[0].clone(), r.clone());
                        Ok(r)
                    }
                    None => {
                        let bbb = type_check_expr(b, context.clone())?;
                        let r = type_elab(context.clone(), Type::Any, bbb.clone())?;
                        unsafe { &mut *context.get() }
                            .type_check_current_scope_variables
                            .insert(aa[0].clone(), r.clone());
                        Ok(r)
                    }
                }
            } else {
                type_check_expr(b, context)
            }
        }
        Expr::SpecifyTyped(ex, ty) => {
            let tyy = type_check_expr(ex, context.clone())?;
            type_elab(context, tyy, ty.clone())
        }
    }
}
