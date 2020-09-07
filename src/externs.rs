use std::collections::HashMap;

use super::ast::Visitor;
use super::extern_impls::*;
use super::interpreter::Interpreter;
use crate::ast::{Info, Prim::*};
use crate::database::Compiler;
use crate::errors::TError;
use crate::types::{
    bit_type, i32_type, number_type, string_type, type_type, unit_type, variable, void_type, Type,
    Type::*,
};

use crate::type_checker::infer;

pub type FuncImpl = Box<dyn Fn(&mut Interpreter, &dyn Compiler, Info) -> Res>;

pub fn get_implementation(name: String) -> Option<FuncImpl> {
    match name.as_str() {
        "print" => Some(Box::new(|interp, db, info| {
            let val = interp.eval_local(db, "print", "it")?;
            match val {
                Str(s, _) => print!("{}", s),
                s => print!("{:?}", s),
            };
            Ok(I32(0, info))
        })),
        "eprint" => Some(Box::new(|interp, db, info| {
            let val = interp.eval_local(db, "eprint", "it")?;
            match val {
                Str(s, _) => eprint!("{}", s),
                s => eprint!("{:?}", s),
            };
            Ok(I32(0, info))
        })),
        "exit" => Some(Box::new(|interp, db, _| {
            let val = interp.eval_local(db, "exit", "it")?;
            let code = match val {
                I32(n, _) => n,
                s => {
                    eprint!("{:?}", s);
                    1
                }
            };
            std::process::exit(code);
        })),
        "!" => Some(Box::new(|interp, db, info| {
            // TODO require 1 arg
            let i = interp.eval_local(db, "!", "it")?;
            match i {
                Bool(n, _) => Ok(Bool(!n, info)),
                _ => Err(TError::TypeMismatch("!".to_string(), Box::new(i), info)),
            }
        })),
        "+" => Some(Box::new(|interp, db, info| {
            // TODO require 1 arg
            let i = interp.eval_local_maybe(db, "+", "it")?;
            match i {
                // TODO require 1 arg
                Some(I32(n, _)) => return Ok(I32(n, info)),
                Some(i) => return Err(TError::TypeMismatch("+".to_string(), Box::new(i), info)),
                _ => {}
            };
            let left = interp.eval_local(db, "+", "left")?;
            let right = interp.eval_local(db, "+", "right")?;
            prim_add(left, right, info)
        })),
        "-" => Some(Box::new(|interp, db, info| {
            // TODO require 1 arg
            let i = interp.eval_local_maybe(db, "-", "it")?;
            match i {
                // TODO require 1 arg
                Some(I32(n, _)) => return Ok(I32(-n, info)),
                Some(i) => return Err(TError::TypeMismatch("-".to_string(), Box::new(i), info)),
                _ => {}
            };
            let left = interp.eval_local(db, "-", "left")?;
            let right = interp.eval_local(db, "-", "right")?;
            prim_sub(left, right, info)
        })),
        "||" => Some(Box::new(|interp, db, info| {
            let left = interp.eval_local(db, "||", "left")?;
            let right = interp.get_local("||", "right")?;
            match (left, right) {
                (Bool(true, _), _) => Ok(Bool(true, info)),
                (Bool(false, _), Bool(r, _)) => Ok(Bool(r, info)),
                (Bool(false, _), Lambda(r)) => interp.visit(db, &mut (), &r),
                (l, r) => Err(TError::TypeMismatch2(
                    "||".to_string(),
                    Box::new(l),
                    Box::new(r),
                    info,
                )),
            }
        })),
        "&&" => Some(Box::new(|interp, db, info| {
            let left = interp.eval_local(db, "&&", "left")?;
            let right = interp.get_local("&&", "right")?;
            match (left, right) {
                (Bool(false, _), _) => Ok(Bool(false, info)),
                (Bool(true, _), Bool(r, _)) => Ok(Bool(r, info)),
                (Bool(true, _), Lambda(r)) => interp.visit(db, &mut (), &r),
                (l, r) => Err(TError::TypeMismatch2(
                    "&&".to_string(),
                    Box::new(l),
                    Box::new(r),
                    info,
                )),
            }
        })),
        "++" => Some(Box::new(|interp, db, info| {
            let left = interp.eval_local(db, "++", "left")?;
            let right = interp.eval_local(db, "++", "right")?;
            prim_add_strs(left, right, info)
        })),
        "==" => Some(Box::new(|interp, db, info| {
            let left = interp.eval_local(db, "==", "left")?;
            let right = interp.eval_local(db, "==", "right")?;
            prim_eq(left, right, info)
        })),
        "!=" => Some(Box::new(|interp, db, info| {
            let left = interp.eval_local(db, "!=", "left")?;
            let right = interp.eval_local(db, "!=", "right")?;
            prim_neq(left, right, info)
        })),
        ">" => Some(Box::new(|interp, db, info| {
            let left = interp.eval_local(db, ">", "left")?;
            let right = interp.eval_local(db, ">", "right")?;
            prim_gt(left, right, info)
        })),
        "<" => Some(Box::new(|interp, db, info| {
            let left = interp.eval_local(db, "<", "left")?;
            let right = interp.eval_local(db, "<", "right")?;
            prim_gt(right, left, info)
        })),
        ">=" => Some(Box::new(|interp, db, info| {
            let left = interp.eval_local(db, ">=", "left")?;
            let right = interp.eval_local(db, ">=", "right")?;
            prim_gte(left, right, info)
        })),
        "<=" => Some(Box::new(|interp, db, info| {
            let left = interp.eval_local(db, "<=", "left")?;
            let right = interp.eval_local(db, "<=", "right")?;
            prim_gte(right, left, info)
        })),
        "*" => Some(Box::new(|interp, db, info| {
            let left = interp.eval_local(db, "*", "left")?;
            let right = interp.eval_local(db, "*", "right")?;
            prim_mul(left, right, info)
        })),
        "/" => Some(Box::new(|interp, db, info| {
            let left = interp.eval_local(db, "/", "left")?;
            let right = interp.eval_local(db, "/", "right")?;
            prim_div(left, right, info)
        })),
        "%" => Some(Box::new(|interp, db, info| {
            let left = interp.eval_local(db, "%", "left")?;
            let right = interp.eval_local(db, "%", "right")?;
            prim_mod(left, right, info)
        })),
        "^" => Some(Box::new(|interp, db, info| {
            let left = interp.eval_local(db, "^", "left")?;
            let right = interp.eval_local(db, "^", "right")?;
            prim_pow(left, right, info)
        })),
        "&" => Some(Box::new(|interp, db, info| {
            let left = interp.eval_local(db, "&", "left")?;
            let right = interp.eval_local(db, "&", "right")?;
            prim_type_and(left, right, info)
        })),
        "|" => Some(Box::new(|interp, db, info| {
            let left = interp.eval_local(db, "|", "left")?;
            let right = interp.eval_local(db, "|", "right")?;
            prim_type_or(left, right, info)
        })),
        ":" => Some(Box::new(|interp, db, info| {
            let value = interp.eval_local(db, "|", "left")?;
            let ty = interp.eval_local(db, "|", "right")?;
            use crate::ast::ToNode;
            let _type_of_value = infer(db, &value.clone().to_node());
            // Check subtyping relationship of type_of_value and ty.
            let sub_type = true;
            if sub_type {
                return Ok(value);
            }
            Err(TError::TypeMismatch2(
                "Failure assertion of type annotation at runtime".to_string(),
                Box::new(value),
                Box::new(ty.clone()),
                ty.get_info(),
            ))
        })),
        "?" => Some(Box::new(|interp, db, info| {
            match interp.eval_local(db, "|", "left") {
                Ok(succ) => Ok(succ),
                Err(_) => interp.eval_local(db, "|", "right"),
            }
        })),
        "-|" => Some(Box::new(|interp, db, info| {
            match interp.eval_local(db, "-|", "left") {
                //TODO: Add pattern matching.
                Ok(Bool(false, info)) => Err(TError::RequirementFailure(info)),
                // Lambda(expr)) => Ok(Lambda(Box::new(expr.clone().to_node()))), // TODO: Worth double checking. Smells
                Ok(_) => interp.eval_local(db, "-|", "right"),
                err => err,
            }
        })),
        ";" => Some(Box::new(|interp, db, _info| {
            interp.eval_local(db, ";", "left")?;
            let lambda = interp.eval_local(db, ";", "right")?;
            interp.visit_prim(db, &mut (), &lambda)
        })),
        "argc" => Some(Box::new(|_interp, db, info| {
            Ok(I32(db.options().interpreter_args.len() as i32, info))
        })),
        "argv" => Some(Box::new(|interp, db, info| {
            let i = interp.eval_local(db, "argv", "it")?;
            match i {
                I32(ind, _) => Ok(Str(
                    db.options().interpreter_args[ind as usize].clone(),
                    info,
                )),
                value => Err(TError::TypeMismatch(
                    "Expected index to be of type i32".to_string(),
                    Box::new(value),
                    info,
                )),
            }
        })),
        "I32" => Some(Box::new(|_, _db, info| Ok(TypeValue(i32_type(), info)))),
        "Number" => Some(Box::new(|_, _db, info| Ok(TypeValue(number_type(), info)))),
        "String" => Some(Box::new(|_, _db, info| Ok(TypeValue(string_type(), info)))),
        "Bit" => Some(Box::new(|_, _db, info| Ok(TypeValue(bit_type(), info)))),
        "Unit" => Some(Box::new(|_, _db, info| Ok(TypeValue(unit_type(), info)))),
        "Void" => Some(Box::new(|_, _db, info| Ok(TypeValue(void_type(), info)))),
        "Type" => Some(Box::new(|_, _db, info| Ok(TypeValue(type_type(), info)))),
        _ => None,
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Direction {
    Left,
    Right,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Semantic {
    Operator {
        binding: i32,
        assoc: Direction,
        laziness: Option<Direction>,
    },
    Func,
}

fn operator(binding: i32, assoc: Direction) -> Semantic {
    Semantic::Operator {
        binding,
        assoc,
        laziness: None,
    }
}

fn lazy_operator(binding: i32, assoc: Direction, laziness: Direction) -> Semantic {
    Semantic::Operator {
        binding,
        assoc,
        laziness: Some(laziness),
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Extern {
    pub name: String,
    pub semantic: Semantic,
    pub ty: Type,
    pub cpp: LangImpl,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct LangImpl {
    pub code: String,
    pub arg_joiner: String,
    pub arg_processor: String,
    pub includes: String,
    pub flags: Vec<String>,
}

impl LangImpl {
    fn new(code: &str) -> LangImpl {
        LangImpl {
            code: code.to_string(),
            arg_joiner: "".to_string(),
            arg_processor: "".to_string(),
            includes: "".to_string(),
            flags: vec![],
        }
    }

    fn operator(arg_joiner: &str) -> LangImpl {
        LangImpl {
            code: "".to_string(),
            arg_joiner: arg_joiner.to_string(),
            arg_processor: "".to_string(),
            includes: "".to_string(),
            flags: vec![],
        }
    }

    fn with_arg_joiner(mut self, arg_joiner: &str) -> LangImpl {
        self.arg_joiner = arg_joiner.to_string();
        self
    }

    fn with_arg_processor(mut self, arg_processor: &str) -> LangImpl {
        self.arg_processor = arg_processor.to_string();
        self
    }

    fn with_includes(mut self, includes: &str) -> LangImpl {
        self.includes = includes.to_string();
        self
    }

    fn with_flag(mut self, flag: &str) -> LangImpl {
        self.flags.push(flag.to_string());
        self
    }
}

pub fn get_externs(_db: &dyn Compiler) -> Result<HashMap<String, Extern>, TError> {
    use Direction::*;
    use Semantic::Func;
    let mut externs = vec![
        Extern {
            name: "argc".to_string(),
            semantic: Func,
            ty: i32_type(),
            cpp: LangImpl::new("argc"),
        },
        Extern {
            name: "argv".to_string(),
            semantic: Func,
            ty: Function {
                results: dict!("it" => string_type()),
                intros: dict!(),
                arguments: dict!("it" => i32_type()),
                effects: vec![],
            },
            cpp: LangImpl::new("([&argv](const int x){return argv[x];})"),
        },
        Extern {
            name: "eprint".to_string(),
            semantic: Func,
            ty: Function {
                results: dict! {},
                arguments: dict! {"it" => string_type()},
                intros: dict!(),
                effects: vec!["stderr".to_string()],
            },
            cpp: LangImpl::new("std::cerr << ").with_includes("#include <iostream>"),
        },
        Extern {
            name: "exit".to_string(),
            semantic: Func,
            ty: Function {
                results: dict! {"it" => void_type()},
                arguments: dict! {"it" => i32_type()},
                intros: dict!(),
                effects: vec!["stderr".to_string()],
            },
            cpp: LangImpl::new("[](const int code){exit(code);}")
                .with_includes("#include <stdlib.h>"),
        },
        Extern {
            name: "print".to_string(),
            semantic: Func,
            ty: Function {
                results: dict! {},
                arguments: dict! {"it" => string_type()},
                intros: dict!(),
                effects: vec!["stdout".to_string()],
            },
            cpp: LangImpl::new("std::cout << ").with_includes("#include <iostream>"),
        },
        Extern {
            name: "pointer".to_string(),
            semantic: Func,
            ty: Function {
                results: dict! {"it" => variable("a")},
                arguments: dict! {"it" => variable("Type")},
                intros: dict!("a" => variable("Type")),
                effects: vec![],
            },
            cpp: LangImpl::new("std::cout << ").with_includes("#include <iostream>"),
        },
        Extern {
            name: ";".to_string(),
            semantic: operator(20, Left),
            ty: Function {
                intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                results: dict!("it" => variable("b")),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator(";"),
        },
        Extern {
            name: ",".to_string(),
            semantic: operator(30, Left),
            ty: Function {
                intros: dict!("a" => variable("Type"), "b" => variable("Type"), "c" => variable("Type")),
                results: dict!("it" => variable("c")),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator(", "),
        },
        Extern {
            name: "=".to_string(),
            semantic: operator(40, Right),
            ty: Function {
                intros: dict!("a" => variable("Identifier"), "b" => variable("Type")),
                results: dict!("it" => variable("b")),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator(" = "),
        },
        Extern {
            name: ":".to_string(),
            semantic: operator(42, Left),
            ty: Function {
                intros: dict!("a" => variable("Type")),
                results: dict!("it" => variable("a")),
                arguments: dict!("left" => variable("a"), "right" => variable("Type")),
                effects: vec![],
            },
            cpp: LangImpl::operator(":"),
        },
        Extern {
            name: "?".to_string(),
            semantic: operator(45, Left),
            ty: Function {
                intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                results: dict!("it" => Union(set!(
                            variable("a"),
                            variable("b")
                    ))
                ),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("?"),
        },
        Extern {
            name: "-|".to_string(),
            semantic: operator(47, Left),
            ty: Function {
                intros: dict!("a" => variable("Type")),
                results: dict!("it" => variable("a")),
                arguments: dict!("left" => variable("Type"), "right" => variable("a")),
                effects: vec![],
            },
            cpp: LangImpl::operator("-|"),
        },
        Extern {
            name: "|".to_string(),
            semantic: operator(48, Left),
            ty: Function {
                intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                results: dict!("it" => Union(set!(variable("a"), variable("b")))),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("|"),
        },
        Extern {
            name: "&".to_string(),
            semantic: operator(48, Left),
            ty: Function {
                intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                results: dict!("it" => Product(set!(variable("a"), variable("b")))),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("&"),
        },
        Extern {
            name: "++".to_string(),
            semantic: operator(49, Left),
            ty: Function {
                intros: dict!("a" => variable("Display"), "b" => variable("Display")),
                results: dict!("it" => string_type()),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("+")
                .with_arg_processor("std::to_string")
                .with_includes(
                    "#include <string>
#include <sstream>
namespace std{
template <typename T>
string to_string(const T& t){
  stringstream out;
  out << t;
  return out.str();
}
string to_string(const bool& t){
  return t ? \"true\" : \"false\";
}
}",
                ),
        },
        Extern {
            name: "<".to_string(),
            semantic: operator(50, Left),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: dict!("it" => bit_type()),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("<"),
        },
        Extern {
            name: "<=".to_string(),
            semantic: operator(50, Left),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: dict!("it" => bit_type()),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("<="),
        },
        Extern {
            name: ">".to_string(),
            semantic: operator(50, Left),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: dict!("it" => bit_type()),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator(">"),
        },
        Extern {
            name: ">=".to_string(),
            semantic: operator(50, Left),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: dict!("it" => bit_type()),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator(">="),
        },
        Extern {
            name: "!=".to_string(),
            semantic: operator(50, Left),
            ty: Function {
                intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                results: dict!("it" => bit_type()),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("!="),
        },
        Extern {
            name: "==".to_string(),
            semantic: operator(50, Left),
            ty: Function {
                intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                results: dict!("it" => bit_type()),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("=="),
        },
        Extern {
            name: "||".to_string(),
            semantic: lazy_operator(60, Left, Right),
            ty: Function {
                intros: dict!(),
                results: dict!("it" => bit_type()),
                arguments: dict!("left" => bit_type(), "right" => bit_type()),
                effects: vec![],
            },
            cpp: LangImpl::operator("||"),
        },
        Extern {
            name: "&&".to_string(),
            semantic: lazy_operator(60, Left, Right),
            ty: Function {
                intros: dict!(),
                results: dict!("it" => bit_type()),
                arguments: dict!("left" => bit_type(), "right" => bit_type()),
                effects: vec![],
            },
            cpp: LangImpl::operator("&&"),
        },
        Extern {
            name: "!".to_string(),
            semantic: operator(70, Left),
            ty: Function {
                intros: dict!(),
                results: dict!("it" => bit_type()),
                arguments: dict!("it" => bit_type()),
                effects: vec![],
            },
            cpp: LangImpl::operator("!"),
        },
        Extern {
            name: "-".to_string(),
            semantic: operator(70, Left),
            ty: Function {
                intros: dict!("a" => variable("Number")),
                results: dict!("it" => variable("a")),
                arguments: dict!("it" => variable("a")),
                effects: vec![],
            },
            cpp: LangImpl::operator("-"),
        },
        Extern {
            name: "+".to_string(),
            semantic: operator(70, Left),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: dict!("it" => variable("a")),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("+"),
        },
        Extern {
            name: "*".to_string(),
            semantic: operator(80, Left),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: dict!("it" => variable("a")),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("*"),
        },
        Extern {
            name: "%".to_string(),
            semantic: operator(80, Left),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: dict!("it" => variable("a")),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("%"),
        },
        Extern {
            name: "/".to_string(),
            semantic: operator(80, Left),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: dict!("it" => variable("a")),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("/"),
        },
        Extern {
            name: "^".to_string(),
            semantic: operator(90, Right),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: dict!("it" => variable("a")),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::new("pow")
                .with_includes("#include <cmath>")
                .with_arg_joiner(", ")
                .with_flag("-lm"),
        },
        Extern {
            name: "Number".to_string(),
            semantic: Func,
            ty: variable("Type"),
            cpp: LangImpl::new("usize"),
        },
        Extern {
            name: "String".to_string(),
            semantic: Func,
            ty: variable("Type"),
            cpp: LangImpl::new("std::string").with_includes("#include <string>"),
        },
        Extern {
            name: "bit_type()".to_string(),
            semantic: Func,
            ty: variable("Type"),
            cpp: LangImpl::new("short"),
        },
        Extern {
            name: "Unit".to_string(),
            semantic: Func,
            ty: variable("Type"),
            cpp: LangImpl::new("void"),
        },
        Extern {
            name: "Void".to_string(),
            semantic: Func,
            ty: variable("Void"),
            cpp: LangImpl::new("/*void: should never happen*/ auto"),
        },
        Extern {
            name: "Type".to_string(),
            semantic: Func,
            ty: variable("Type"),
            cpp: LangImpl::new("auto"),
        },
    ];
    let mut extern_map: HashMap<String, Extern> = HashMap::new();
    while let Some(extern_def) = externs.pop() {
        extern_map.insert(extern_def.name.clone(), extern_def);
    }
    Ok(extern_map)
}
