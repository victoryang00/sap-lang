use core::cell::UnsafeCell;

use alloc::{boxed::Box, collections::BTreeMap, rc::Rc, vec};

use crate::parser::{
    expr::{CommentedExpr, Expr},
    ty::Type,
    TopLevel,
};

use super::TypeCheckContext;

pub fn type_elab(
    context: Rc<UnsafeCell<TypeCheckContext>>,
    a: Type,
    b: Type,
) -> Result<Type, &'static str> {
    match (a.clone(), b.clone()) {
        (Type::Any, _) => Ok(b),
        (_, Type::Any) => Ok(a),
        (_, Type::Alias(alias)) => match unsafe { &mut *context.get() }.get_alias(&alias) {
            Some(b) => type_elab(context, a, b),
            None => Err("type mismatch"),
        },
        (Type::Alias(alias), b) => match unsafe { &mut *context.get() }.get_alias(&alias) {
            Some(a) => type_elab(context, a, b),
            None => Err("type mismatch"),
        },
        // number
        (Type::Number, Type::Number) => Ok(a),

        // string
        (Type::String, Type::String) => Ok(a),

        // bool
        (Type::Bool, Type::Bool) => Ok(a),

        // function
        (Type::Function(args1, ret1), Type::Function(args2, ret2)) => {
            match args1.len().cmp(&args2.len()) {
                core::cmp::Ordering::Equal => {
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
                    return Err("type mismatch");
                }
            }
        }
        // array
        (Type::Array, Type::Array) => Ok(a),
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
        (Type::Enum(aa), Type::Enum(b)) => {
            for t in b {
                if aa
                    .iter()
                    .position(|x| type_elab(context.clone(), x.clone(), t.clone()).is_ok())
                    .is_some()
                {
                } else {
                    return Err("type mismatch");
                }
            }
            Ok(a)
        }
        (Type::Enum(us), t) => {
            if us
                .iter()
                .position(|x| type_elab(context.clone(), x.clone(), t.clone()).is_ok())
                .is_some()
            {
                Ok(a)
            } else {
                Err("type mismatch")
            }
        }
        (a, b) => {
            // todo!("what happend with {:?} {:?}", a, b)
            Ok(Type::Any)
        }
    }
}
