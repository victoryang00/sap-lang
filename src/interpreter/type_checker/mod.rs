use std::{
    cell::UnsafeCell,
    collections::{BTreeMap, HashMap},
    rc::Rc,
};

use crate::parser::ty::Type;

#[derive(Debug)]
pub struct TypeCheckContext {
    pub(crate) parent: Option<Rc<UnsafeCell<TypeCheckContext>>>,
    pub(crate) free_var: HashMap<String, Type>,
    pub(crate) alias: HashMap<String, Type>,
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
            free_var: HashMap::new(),
            alias: HashMap::new(),
        }
    }
}

pub fn type_elab(context: Rc<UnsafeCell<TypeCheckContext>>, a: Type, b: Type) -> Result<Type, ()> {
    match (a.clone(), b.clone()) {
        (Type::Any, _) => Ok(b),
        (_, Type::Any) => Ok(a),
        (_, Type::Alias(alias)) => match unsafe { &mut *context.get() }.get_alias(&alias) {
            Some(b) => type_elab(context, a, b),
            None => Err(()),
        },
        (Type::Alias(alias), b) => match unsafe { &mut *context.get() }.get_alias(&alias) {
            Some(a) => type_elab(context, a, b),
            None => Err(()),
        },
        // number
        (Type::Number, Type::Number) => Ok(a),
        (Type::Number, _) => {
            return Err(());
        }
        // string
        (Type::String, Type::String) => Ok(a),
        (Type::String, _) => {
            return Err(());
        }
        // bool
        (Type::Bool, Type::Bool) => Ok(a),

        (Type::Bool, _) => {
            return Err(());
        }
        // function
        (Type::Function(args1, ret1), Type::Function(args2, ret2)) => {
            match args1.len().cmp(&args2.len()) {
                std::cmp::Ordering::Equal => {
                    let mut args = vec![];
                    for i in 0..args1.len() {
                        args.push(type_elab(
                            context.clone(),
                            args1[i].clone(),
                            args2[i].clone(),
                        )?);
                    }
                    let r = type_elab(context, *ret1, *ret2)?;
                    Ok(Type::Function(args, Box::new(r)))
                }
                _ => {
                    return Err(());
                }
            }
        }
        (Type::Function(_, _), _) => {
            return Err(());
        }
        // array
        (Type::Array, Type::Array) => Ok(a),
        (Type::Array, _) => {
            return Err(());
        }
        // object
        (Type::Object(ass), Type::Object(bss)) => {
            let mut m = BTreeMap::new();
            for (k, v) in ass {
                m.insert(k.clone(), v.clone());
            }
            for (k, v) in bss {
                if m.contains_key(&k) {
                    return type_elab(context, m.get_mut(&k).unwrap().clone(), v);
                } else {
                    m.insert(k.clone(), v.clone());
                }
            }
            Ok(Type::Object(m))
        }
        // enum
        (Type::Enum(_), Type::Enum(_)) => todo!(),
        (Type::Enum(_), _) => todo!(),
        (a, b) => {
            todo!("what happend with {:?} {:?}", a, b)
        }
    }
}
