use std::collections::HashMap;

use crate::ast::{Info, Prim::*};
use crate::database::Compiler;
use crate::errors::TError;
use super::extern_impls::*;
use crate::types::{
    bit_type, i32_type, number_type, string_type, type_type, unit_type, variable, void_type, Type,
    Type::*,
};

pub type FuncImpl = Box<dyn Fn(&dyn Compiler, &mut State, Info) -> Res>;

/*
fn visit_bin_op(db: &dyn Compiler, expr: &BinOp) -> Res {
    use Prim::*;
    if db.debug() > 1 {
        eprintln!("evaluating binop {}", expr.clone().to_node());
    }
    let info = expr.clone().get_info();
    let l = self.visit(db, state, &expr.left);
    let mut r = || self.visit(db, state, &expr.right);
    match expr.name.as_str() {
        "+" => prim_add(&l?, &r()?, info),
        "++" => prim_add_strs(&l?, &r()?, info),
        "==" => prim_eq(&l?, &r()?, info),
        "!=" => prim_neq(&l?, &r()?, info),
        ">" => prim_gt(&l?, &r()?, info),
        "<" => prim_gt(&r()?, &l?, info),
        ">=" => prim_gte(&l?, &r()?, info),
        "<=" => prim_gte(&r()?, &l?, info),
        "-" => prim_sub(&l?, &r()?, info),
        "*" => prim_mul(&l?, &r()?, info),
        "/" => prim_div(&l?, &r()?, info),
        "%" => prim_mod(&l?, &r()?, info),
        "^" => prim_pow(&l?, &r()?, info),
        "||" => prim_or(&l?, &r()?, info),
        "&" => prim_type_and(l?, r()?, info),
        "|" => prim_type_or(l?, r()?, info),
        ";" => {
            l?;
            Ok(r()?)
        }
        "?" => match l {
            Err(_) => r(),
            l => l,
        },
        "-|" => match l {
            //TODO: Add pattern matching.
            Ok(Bool(false, info)) => Err(TError::RequirementFailure(info)),
            Ok(Lambda(_)) => Ok(Lambda(Box::new(expr.clone().to_node()))),
            Ok(_) => r(),
            l => l,
        },
        ":" => {
            let value = l?;
            let ty = r()?;
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
        }
        op => Err(TError::UnknownInfixOperator(op.to_string(), info)),
    }
}
*/

pub fn get_implementation(name: String) -> Option<FuncImpl> {
    match name.as_str() {
        "print" => Some(Box::new(|_, state, info| {
            let val = get_local(state, "print", "it");
            match val {
                Str(s, _) => print!("{}", s),
                s => print!("{:?}", s),
            };
            Ok(I32(0, info))
        })),
        "eprint" => Some(Box::new(|_, state, info| {
            let val = get_local(state, "eprint", "it");
            match val {
                Str(s, _) => eprint!("{}", s),
                s => eprint!("{:?}", s),
            };
            Ok(I32(0, info))
        })),
        "exit" => Some(Box::new(|_, state, _| {
            let val = get_local(state, "exit", "it");
            let code = match val {
                I32(n, _) => n,
                s => {
                    eprint!("{:?}", s);
                    1
                }
            };
            std::process::exit(code);
        })),
        "!" => Some(Box::new(|_, state, info| {
            // TODO require 1 arg
            let i = get_local(state, "!", "it");
            match i {
                Bool(n, _) => Ok(Bool(!n, info)),
                _ => Err(TError::TypeMismatch("!".to_string(), Box::new(i), info)),
            }
        })),
        "+" =>  Some(Box::new(|_, state, info| {
            // TODO require 1 arg
            let i = get_local(state, "+", "it");
            match i {
                // TODO require 1 arg
                I32(n, _) => Ok(I32(n, info)),
                _ => Err(TError::TypeMismatch("+".to_string(), Box::new(i), info)),
            }
        })),
        "-" => Some(Box::new(|_, state, info| {
            // TODO require 1 arg
            let i = get_local(state, "-", "it");
            match i {
                // TODO require 1 arg
                I32(n, _) => Ok(I32(-n, info)),
                _ => Err(TError::TypeMismatch("-".to_string(), Box::new(i), info)),
            }
        })),
        "&&" => Some(Box::new(|_, state, info| {
            let left = get_local(state, "&&", "left");
            let right = get_local(state, "&&", "right");
            prim_and(&left, &right, info)
        })),
        "++" => Some(Box::new(|_, state, info| {
            let left = get_local(state, "++", "left");
            let right = get_local(state, "++", "right");
            prim_add_strs(&left, &right, info)
        })),
        "^" => Some(Box::new(|_, state, info| {
            let left = get_local(state, "^", "left");
            let right = get_local(state, "^", "right");
            prim_pow(&left, &right, info)
        })),
        "argc" => Some(Box::new(|db, _state, info| {
            Ok(I32(db.options().interpreter_args.len() as i32, info))
        })),
        "argv" => Some(Box::new(|db, state, info| {
            let i = get_local(state, "argv", "it");
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
        "I32" => Some(Box::new(|_db, _, info| Ok(TypeValue(i32_type(), info)))),
        "Number" => Some(Box::new(|_db, _, info| Ok(TypeValue(number_type(), info)))),
        "String" => Some(Box::new(|_db, _, info| Ok(TypeValue(string_type(), info)))),
        "Bit" => Some(Box::new(|_db, _, info| Ok(TypeValue(bit_type(), info)))),
        "Unit" => Some(Box::new(|_db, _, info| Ok(TypeValue(unit_type(), info)))),
        "Void" => Some(Box::new(|_db, _, info| Ok(TypeValue(void_type(), info)))),
        "Type" => Some(Box::new(|_db, _, info| Ok(TypeValue(type_type(), info)))),
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
    Operator { binding: i32, assoc: Direction },
    Func,
}

fn operator(binding: i32, assoc: Direction) -> Semantic {
    Semantic::Operator { binding, assoc }
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

pub fn get_externs(db: &dyn Compiler) -> Result<HashMap<String, Extern>, TError> {
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
            semantic: operator(60, Left),
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
            semantic: operator(60, Left),
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
