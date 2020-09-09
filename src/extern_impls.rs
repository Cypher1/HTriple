use std::collections::HashMap;
use super::ast::*;
use super::errors::TError;

pub type Res = Result<Prim, TError>;
pub type Frame = HashMap<String, Prim>;
pub type State = Vec<Frame>;

pub fn get_local(state: &mut State, function_name: &str, name: &str) -> Prim {
    if let Some(frame) = state.last() {
        if let Some(val) = frame.get(name) {
            return val.clone();
        }
        panic!("{} needs argument named {}", function_name, name)
    }
    panic!("no frame for function {}", function_name)
}

pub fn prim_add(l: &Prim, r: &Prim, info: Info) -> Res {
    use crate::types::sum;
    use Prim::*;
    match (l, r) {
        (Bool(l, _), Bool(r, _)) => Ok(I32(if *l { 1 } else { 0 } + if *r { 1 } else { 0 }, info)),
        (Bool(l, _), I32(r, _)) => Ok(I32(r.wrapping_add(if *l { 1 } else { 0 }), info)),
        (Bool(l, _), Str(r, _)) => Ok(Str(l.to_string() + &r.to_string(), info)),
        (I32(l, _), Bool(r, _)) => Ok(I32(l.wrapping_add(if *r { 1 } else { 0 }), info)),
        (I32(l, _), I32(r, _)) => Ok(I32(l.wrapping_add(*r), info)),
        (I32(l, _), Str(r, _)) => Ok(Str(l.to_string() + &r.to_string(), info)),
        (Str(l, _), Bool(r, _)) => Ok(Str(l.to_string() + &r.to_string(), info)),
        (Str(l, _), I32(r, _)) => Ok(Str(l.to_string() + &r.to_string(), info)),
        (Str(l, _), Str(r, _)) => Ok(Str(l.to_string() + &r.to_string(), info)),
        (TypeValue(l, _), TypeValue(r, _)) => Ok(TypeValue(sum(vec![l.clone(), r.clone()])?, info)),
        (l, r) => Err(TError::TypeMismatch2(
            "+".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

pub fn prim_add_strs(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    let to_str = |val: &Prim| match val {
        Bool(v, _) => format!("{}", v),
        I32(v, _) => format!("{}", v),
        Str(v, _) => v.clone(),
        Lambda(v) => format!("{}", v),
        TypeValue(v, _) => format!("{}", v),
    };
    Ok(Str(to_str(l) + &to_str(r), info))
}

pub fn prim_eq(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (Bool(l, _), Bool(r, _)) => Ok(Bool(*l == *r, info)),
        (I32(l, _), I32(r, _)) => Ok(Bool(l == r, info)),
        (Str(l, _), Str(r, _)) => Ok(Bool(l == r, info)),
        (l, r) => Err(TError::TypeMismatch2(
            "==".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

pub fn prim_neq(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (Bool(l, _), Bool(r, _)) => Ok(Bool(*l != *r, info)),
        (I32(l, _), I32(r, _)) => Ok(Bool(l != r, info)),
        (Str(l, _), Str(r, _)) => Ok(Bool(l != r, info)),
        (l, r) => Err(TError::TypeMismatch2(
            "!=".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

pub fn prim_gt(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (Bool(l, _), Bool(r, _)) => Ok(Bool(*l & !(*r), info)),
        (I32(l, _), I32(r, _)) => Ok(Bool(l > r, info)),
        (Str(l, _), Str(r, _)) => Ok(Bool(l > r, info)),
        (l, r) => Err(TError::TypeMismatch2(
            ">".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

pub fn prim_gte(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (Bool(l, _), Bool(r, _)) => Ok(Bool(*l >= *r, info)),
        (I32(l, _), I32(r, _)) => Ok(Bool(l >= r, info)),
        (Str(l, _), Str(r, _)) => Ok(Bool(l >= r, info)),
        (l, r) => Err(TError::TypeMismatch2(
            ">=".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

pub fn prim_sub(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (I32(l, _), Bool(r, _)) => Ok(I32(l - if *r { 1 } else { 0 }, info)),
        (I32(l, _), I32(r, _)) => Ok(I32(l - r, info)),
        (l, r) => Err(TError::TypeMismatch2(
            "-".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

pub fn prim_mul(l: &Prim, r: &Prim, info: Info) -> Res {
    use crate::types::record;
    use Prim::*;
    match (l, r) {
        (Bool(l, _), I32(r, _)) => Ok(I32(if *l { *r } else { 0 }, info)),
        (Bool(l, _), Str(r, _)) => Ok(Str(if *l { r.to_string() } else { "".to_string() }, info)),
        (I32(l, _), Bool(r, _)) => Ok(I32(if *r { *l } else { 0 }, info)),
        (I32(l, _), I32(r, _)) => Ok(I32(l.wrapping_mul(*r), info)),
        (Str(l, _), Bool(r, _)) => Ok(Str(if *r { l.to_string() } else { "".to_string() }, info)),
        (TypeValue(l, _), TypeValue(r, _)) => {
            Ok(TypeValue(record(vec![l.clone(), r.clone()])?, info))
        }
        (l, r) => Err(TError::TypeMismatch2(
            "*".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

pub fn prim_div(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (I32(l, _), I32(r, _)) => Ok(I32(l / r, info)),
        (l, r) => Err(TError::TypeMismatch2(
            "/".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

pub fn prim_mod(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (I32(l, _), I32(r, _)) => Ok(I32(l % r, info)),
        (l, r) => Err(TError::TypeMismatch2(
            "%".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

pub fn prim_and(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (Bool(l, _), Bool(r, _)) => Ok(Bool(*l && *r, info)),
        (l, r) => Err(TError::TypeMismatch2(
            "&&".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

pub fn prim_or(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (Bool(l, _), Bool(r, _)) => Ok(Bool(*l || *r, info)),
        (l, r) => Err(TError::TypeMismatch2(
            "||".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

pub fn prim_type_and(l: Prim, r: Prim, info: Info) -> Res {
    use crate::types::Type;
    use Prim::*;
    match (l, r) {
        (TypeValue(l, _), TypeValue(r, _)) => Ok(TypeValue(Type::Product(set!(l, r)), info)),
        (l, r) => Err(TError::TypeMismatch2(
            "&".to_string(),
            Box::new(l),
            Box::new(r),
            info,
        )),
    }
}

pub fn prim_type_or(l: Prim, r: Prim, info: Info) -> Res {
    use crate::types::Type;
    use Prim::*;
    match (l, r) {
        (TypeValue(l, _), TypeValue(r, _)) => Ok(TypeValue(Type::Union(set!(l, r)), info)),
        (l, r) => Err(TError::TypeMismatch2(
            "|".to_string(),
            Box::new(l),
            Box::new(r),
            info,
        )),
    }
}

pub fn prim_pow(l: &Prim, r: &Prim, info: Info) -> Res {
    use Prim::*;
    match (l, r) {
        (I32(l, _), Bool(r, _)) => Ok(I32(if *r { *l } else { 1 }, info)),
        (I32(l, _), I32(r, _)) => Ok(I32(i32::pow(*l, *r as u32), info)), // TODO: require pos pow
        (l, r) => Err(TError::TypeMismatch2(
            "^".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info,
        )),
    }
}

