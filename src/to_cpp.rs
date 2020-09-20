use crate::ast::*;
use crate::{database::Compiler, errors::TError};
use std::collections::HashSet;

// Walks the AST compiling it to wasm.
#[derive(Default)]
pub struct CodeGenerator {
    functions: Vec<Code>,
    includes: HashSet<String>,
    pub flags: HashSet<String>,
}

#[derive(Clone, Debug)]
pub enum Code {
    Empty,
    Block(Vec<Code>),
    Expr(String),
    Statement(String),
    If {
        condition: Box<Code>,
        then: Box<Code>,
        then_else: Box<Code>,
    },
    Func {
        name: String,
        args: Vec<String>,
        return_type: String,
        body: Box<Code>,
        lambda: bool,
    },
}

impl Code {
    fn with_expr(self: Code, f: &dyn Fn(String) -> Code) -> Code {
        match self {
            Code::Empty => Code::Empty,
            Code::Expr(expr) => f(expr),
            Code::Block(mut statements) => {
                let last = statements.pop().unwrap();
                statements.push(last.with_expr(f));
                Code::Block(statements)
            }
            Code::Statement(line) => Code::Statement(line),
            Code::If {
                condition,
                then,
                then_else,
            } => Code::If {
                condition,
                then,
                then_else,
            },
            Code::Func {
                name,
                args,
                mut body,
                lambda,
                return_type,
            } => {
                body = Box::new(body.with_expr(f));
                Code::Func {
                    name,
                    args,
                    body,
                    lambda,
                    return_type,
                }
            }
        }
    }

    fn merge(self: Code, other: Code) -> Code {
        match (self, other) {
            (Code::Empty, right) => right,
            (left, Code::Empty) => left,
            (Code::Block(mut left), Code::Block(right)) => {
                left.extend(right);
                Code::Block(left)
            }
            (mut left, Code::Block(mut right)) => {
                if let Code::Expr(expr) = left {
                    left = Code::Statement(expr);
                }
                right.insert(0, left);
                Code::Block(right) // Backwards?
            }
            (Code::Block(mut left), right) => {
                for line in left.iter_mut() {
                    if let Code::Expr(expr) = line {
                        *line = Code::Statement(expr.to_owned());
                    }
                }
                left.push(right);
                Code::Block(left)
            }
            (mut left, right) => {
                if let Code::Expr(expr) = left {
                    left = Code::Statement(expr);
                }
                Code::Block(vec![left, right])
            }
        }
    }
}

pub fn make_name(def: Vec<Symbol>) -> String {
    let def_n: Vec<String> = def.iter().map(|n| n.clone().to_name()).collect();
    def_n.join("_")
}

fn pretty_print_block(src: Code, indent: &str) -> String {
    // Calculate the expression as well...
    // TODO: Consider if it is dropped (should it be stored? is it a side effect?)
    match src {
        Code::Block(statements) => {
            let new_indent = indent.to_string() + "  ";
            let body: Vec<String> = statements
                .iter()
                .map(|x| pretty_print_block(x.clone(), &new_indent))
                .collect();
            format!("{{{}{indent}}}", body.join(""), indent = indent,)
        }
        Code::Expr(line) => line,
        Code::Statement(line) => format!("{}{};", indent, line),
        Code::Empty => "".to_string(),
        Code::If {
            condition,
            then,
            then_else,
        } => {
            let cond = pretty_print_block(*condition, &indent);
            let body = pretty_print_block(*then, &indent);
            let then_else = pretty_print_block(*then_else, &indent);
            format!(
                "{indent}if({}) {} else {}",
                cond,
                body,
                then_else,
                indent = indent,
            )
        }
        Code::Func {
            name,
            args,
            return_type,
            body: inner,
            lambda,
        } => {
            let body = if let Code::Block(_) = *inner {
                pretty_print_block(*inner, &indent)
            } else {
                // Auto wrap statements in blocks.
                pretty_print_block(Code::Block(vec![*inner]), &indent)
            };
            if lambda {
                format!(
                    "{indent}const auto {} = [&]({}) {};",
                    name,
                    args.join(", "),
                    body,
                    indent = indent
                )
            } else {
                format!(
                    "{indent}{} {}({}) {}",
                    return_type,
                    name,
                    args.join(", "),
                    body,
                    indent = indent
                )
            }
        }
    }
}

type Res = Result<Code, TError>;
type State = Table;
type Out = (String, HashSet<String>);

impl CodeGenerator {
    fn build_call(&mut self, before: &str, mid: &str, args: Vec<Code>) -> Code {
        match args.len() {
            0 => Code::Expr(format!("{}()", before)),
            1 => args[0]
                .clone()
                .with_expr(&|exp| Code::Expr(format!("{}({})", before, exp))),
            _ => {
                let mut content: Code = args[0].clone().with_expr(&|exp| Code::Expr(exp));
                for arg in args[1..].iter() {
                    content = content.with_expr(&|content| {
                        arg.clone().with_expr(&|arg_expr| {
                            Code::Expr(format!("{}{}{}", content, mid, arg_expr))
                        })
                    });
                }
                content.with_expr(&|content| Code::Expr(format!("{}({})", before, content)))
            }
        }
    }
}

impl Visitor<State, Code, Out, Path> for CodeGenerator {
    fn visit_root(&mut self, db: &dyn Compiler, module: &Path) -> Result<Out, TError> {
        let root = db.look_up_definitions(module.clone())?;
        let mut main_info = root.ast.get_info();
        let mut main_at = module.clone();
        main_at.push(Symbol::new("main".to_string()));
        main_info.defined_at = Some(main_at);
        let main_let = Let {
            info: main_info,
            name: "main".to_string(),
            value: Box::new(root.ast.clone()),
            args: Some(vec![]),
        };
        let mut table = root.table; // TODO: Shouldn't be mut
        if db.debug() > 1 {
            eprintln!("table {:?}", table);
        }
        let main = match self.visit_let(db, &mut table, &main_let)? {
            Code::Func {
                name: _,
                args: _,
                body,
                lambda: _,
                return_type: _,
            } => Code::Func {
                name: "main".to_string(),
                args: vec!["int argc".to_string(), "char* argv[]".to_string()],
                body,
                lambda: false,
                return_type: "int".to_string(),
            },
            thing => panic!("main must be a Func {:?}", thing),
        };
        // TODO(cypher1): Use a writer.
        let mut code = "".to_string();

        // #includes
        let mut includes: Vec<&String> = self.includes.iter().collect();
        includes.sort();
        for inc in includes.iter() {
            if inc.as_str() != "" {
                code = format!("{}{}\n", code, inc);
            }
        }
        // Forward declarations
        for func in self.functions.clone().iter() {
            match &func {
                Code::Func { name, args, .. } => {
                    code = format!("{}{}({});\n", code, name, args.join(", "))
                }
                _ => panic!("Cannot create function from non-function"),
            }
        }

        self.functions.push(main);

        // Definitions
        for func in self.functions.iter().clone() {
            let function = pretty_print_block(func.to_owned(), "\n");
            code = format!("{}{}", code, function);
        }
        Ok((code + "\n", self.flags.clone()))
    }

    fn visit_sym(&mut self, db: &dyn Compiler, _state: &mut State, expr: &Sym) -> Res {
        // eprintln!(
        //   "to_c: visit {}, {:?}",
        // expr.name,
        //   expr.get_info().defined_at
        // );
        let name = make_name(
            expr.get_info()
                .defined_at
                .expect("Could not find definition for symbol"),
        );
        if let Some(info) = db.get_extern(name.clone())? {
            self.includes.insert(info.cpp.includes);
            self.flags.extend(info.cpp.flags);
            // arg_processor
            return Ok(Code::Expr(info.cpp.code));
        }
        Ok(Code::Expr(name))
    }

    fn visit_prim(&mut self, db: &dyn Compiler, state: &mut State, expr: &Prim) -> Res {
        eprintln!("To Cpp prim: {}", expr.clone().to_node());
        use Prim::*;
        match expr {
            I32(n, _) => Ok(Code::Expr(n.to_string())),
            Bool(true, _) => Ok(Code::Expr(1.to_string())),
            Bool(false, _) => Ok(Code::Expr(0.to_string())),
            Str(s, _) => Ok(Code::Expr(format!("{:?}", s))),
            Lambda(node) => self.visit(db, state, node),
            TypeValue(_ty, _) => {
                unimplemented!("unimplemented primitive type in compilation to cpp")
            }
        }
    }

    fn visit_apply(&mut self, db: &dyn Compiler, state: &mut State, expr: &Apply) -> Res {
        // eprintln!("apply here: {:?}", expr);
        if let Node::SymNode(sym_node) = &*expr.inner {
            let op = sym_node.name.as_str();

            match op {
                "-|" => {
                    // TODO: handle 'error' values more widly.
                    if expr.args.len() != 2 {
                        panic!("TODO");
                    }
                    let (left, right) = (expr.args[0].clone(), expr.args[1].clone());
                    let (left, right) = (
                        self.visit_let(db, state, &left.clone())?,
                        self.visit_let(db, state, &right.clone())?,
                    );
                    let done = Code::If {
                        condition: Box::new(left),
                        then: Box::new(right),
                        then_else: Box::new(Code::Statement("throw 101".to_string())),
                    };
                    return Ok(done);
                }
                ";" => {
                    if expr.args.len() != 2 {
                        panic!("TODO");
                    }
                    let (left, right) = (expr.args[0].clone(), expr.args[1].clone());
                    let (left, right) = (
                        self.visit_let(db, state, &left.clone())?,
                        self.visit_let(db, state, &right.clone())?,
                    );
                    // TODO: handle 'error' values more widly.
                    // TODO: ORDERING
                    return Ok(left.merge(right));
                }
                _ => {}
            }
            if let Some(info) = db.get_extern(op.to_string())? {
                let mut arg_exprs = vec![];
                for arg in expr.args.iter() {
                    let arg: Code = self.visit_let(db, state, &arg.clone())?;
                    let arg: Code = if info.cpp.arg_processor.as_str() == "" {
                        arg
                    } else {
                        self.build_call(info.cpp.arg_processor.as_str(), "", vec![arg])
                    };
                    arg_exprs.push(arg);
                }
                self.includes.insert(info.cpp.includes);
                self.flags.extend(info.cpp.flags);
                return Ok(self.build_call(
                    info.cpp.code.as_str(),
                    info.cpp.arg_joiner.as_str(),
                    arg_exprs,
                ));
            }
        }
        let mut arg_exprs = vec![];
        for arg in expr.args.iter() {
            let body = self.visit(db, state, &arg.value)?;
            if let Some(args) = &arg.args {
                let body = body.with_expr(&|exp| Code::Statement(format!("return {}", exp)));
                let arg_expr = pretty_print_block(body, "");
                let mut arg_names: Vec<String> = vec![];
                for lambda_arg in args.iter() {
                    arg_names.push(format!(
                        "const auto {}",
                        pretty_print_block(self.visit_sym(db, state, lambda_arg)?, "")
                    ));
                }
                arg_exprs.push(format!("[&]({}){{{}}}", arg_names.join(", "), arg_expr));
                continue;
            }
            let arg_expr = pretty_print_block(body, "");
            arg_exprs.push(arg_expr.clone())
        }
        // TODO: require label is none.
        let arg_str = arg_exprs.join(", ");
        match self.visit(db, state, &expr.inner)? {
            Code::Expr(expr) => {
                let with_args = format!("{}({})", expr, arg_str);
                Ok(Code::Expr(with_args))
            }
            _ => panic!("Don't know how to apply arguments to a block"),
        }
    }

    fn visit_let(&mut self, db: &dyn Compiler, state: &mut State, expr: &Let) -> Res {
        eprintln!("To Cpp let: {}", expr.clone().to_node());
        let filename = expr
            .get_info()
            .loc
            .expect("cannot find symbol location")
            .filename
            .expect("cannot find symbol file location");

        let context = db.module_name(filename);

        let path = expr.get_info().defined_at.expect("Undefined symbol")[context.len()..].to_vec();

        let uses = db
            .find_symbol_uses(context.clone(), path.clone())?
            .unwrap_or_else(|| panic!("couldn't find {:?} {:?}", context.clone(), path.clone()));
        if true /*uses.is_empty()*/ {
            eprintln!("culling let: {:?}", expr.get_info().defined_at);
            return Ok(Code::Empty);
        }
        let name = make_name(
            expr.get_info()
                .defined_at
                .expect("Could not find definition for let"),
        );
        let body = self.visit(db, state, &expr.value)?;
        if let Some(args) = &expr.args {
            let body = body.with_expr(&|exp| Code::Statement(format!("return {}", exp)));
            let args: Vec<String> = args
                .iter()
                .map(|s| {
                    format!(
                        "const auto {}",
                        make_name(
                            s.get_info()
                                .defined_at
                                .expect("Could not find definition for let argument"),
                        )
                    )
                })
                .collect();

            let node = Code::Func {
                name,
                args,
                return_type: "int".to_string(),
                body: Box::new(body),
                lambda: true,
            };

            return Ok(node);
        }
        Ok(body.with_expr(&|x| Code::Statement(format!("const auto {} = {}", name, x))))
    }

    fn handle_error(&mut self, _db: &dyn Compiler, _state: &mut State, expr: &Err) -> Res {
        Err(TError::FailedParse(expr.msg.clone(), expr.get_info()))
    }
}
