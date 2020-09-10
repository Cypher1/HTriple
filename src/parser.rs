use std::collections::VecDeque;
use std::sync::Arc;

use super::ast::*;
use super::database::Compiler;
use super::errors::TError;
use super::externs::{Direction, Semantic};
use super::location::*;
use super::tokens::*;

fn binding(db: &dyn Compiler, tok: &Token) -> Result<Semantic, TError> {
    db.get_extern_operator(tok.value.to_owned())
}

fn binding_dir(db: &dyn Compiler, tok: &Token) -> Result<Direction, TError> {
    Ok(match binding(db, tok)? {
        Semantic::Operator { assoc, .. } => assoc,
        Semantic::Func => Direction::Left,
    })
}

fn binding_power(db: &dyn Compiler, tok: &Token) -> Result<i32, TError> {
    Ok(match binding(db, tok)? {
        Semantic::Operator { binding, .. } => binding,
        Semantic::Func => 1000,
    })
}

fn binding_laziness(db: &dyn Compiler, tok: &Token) -> Result<bool, TError> {
    Ok(match binding(db, tok)? {
        Semantic::Operator { laziness, .. } => laziness,
        Semantic::Func => false,
    })
}

fn get_defs(root: Node) -> Vec<Let> {
    use Node::*;
    let mut all_args = vec![];

    match root {
        LetNode(n) => all_args.push(n),
        SymNode(n) => all_args.push(Let {
            name: n.name.clone(),
            args: None,
            info: n.get_info(),
            value: Box::new(n.to_node()),
        }),
        ApplyNode(Apply {
            inner,
            args,
            info,
        }) => {
            if SymNode(Sym{name: ",".to_string(), info: Info::default()}) == *inner {
                for arg in args {
                    all_args.append(&mut get_defs(arg.to_node()));
                }
            } else {
                all_args.push(Let {
                    name: "it".to_string(),
                    args: None,
                    value: Box::new(
                        Apply{inner, args, info: info.clone()}.to_node()
                        .clone()),
                    info,
                });
            }
        },
        n => all_args.push(Let {
            name: "it".to_string(),
            args: None,
            info: n.get_info(),
            value: Box::new(n),
        }),
    }
    all_args
}

impl Token {
    pub fn get_info(&self) -> Info {
        self.pos.clone().get_info()
    }
}

impl Loc {
    pub fn get_info(self) -> Info {
        Info {
            loc: Some(self),
            ..Info::default()
        }
    }
}

fn nud(db: &dyn Compiler, mut toks: VecDeque<Token>) -> Result<(Node, VecDeque<Token>), TError> {
    if let Some(head) = toks.pop_front() {
        match head.tok_type {
            TokenType::NumLit => Ok((
                Prim::I32(head.value.parse().unwrap(), head.get_info()).to_node(),
                toks,
            )),
            TokenType::StringLit => Ok((
                Prim::Str(head.value.clone(), head.get_info()).to_node(),
                toks,
            )),
            TokenType::Op => {
                let lbp = binding_power(db, &head)?;
                let (right, new_toks) = expr(db, toks, lbp)?;
                Ok((
                    un_op(
                        head.value.as_str(),
                        right,
                        head.get_info(),
                    ),
                    new_toks,
                ))
            }
            TokenType::CloseBracket => {
                panic!("Unexpected close bracket {}", head.value);
            }
            TokenType::OpenBracket => {
                let (inner, mut new_toks) = expr(db, toks, 0)?;
                // TODO require close bracket.
                let close = new_toks.front();
                match (head.value.as_str(), close) {
                    (
                        open,
                        Some(Token {
                            value: close,
                            tok_type: TokenType::CloseBracket,
                            pos,
                        }),
                    ) => {
                        match (open, close.as_str()) {
                            ("(", ")") => {}
                            ("[", "]") => {}
                            ("{", "}") => {}
                            (open, chr) => {
                                panic!(format!(
                                    "Unexpected closing bracket for {}, found {} at {:?}.",
                                    open, chr, pos
                                ));
                            }
                        };
                    }
                    (open, chr) => {
                        panic!("Unclosed bracket {} found {:?}", open, chr);
                    }
                }
                new_toks.pop_front();
                Ok((inner, new_toks))
            }
            TokenType::Sym => {
                // TODO: Consider making these globals.
                if head.value == "true" {
                    return Ok((Prim::Bool(true, head.get_info()).to_node(), toks));
                }
                if head.value == "false" {
                    return Ok((Prim::Bool(false, head.get_info()).to_node(), toks));
                }
                Ok((
                    Sym {
                        name: head.value.clone(),
                        info: head.get_info(),
                    }
                    .to_node(),
                    toks,
                ))
            }
            TokenType::Unknown | TokenType::Whitespace => {
                panic!("Lexer should not produce unknown or whitespace")
            }
        }
    } else {
        Ok((
            Err {
                msg: "Unexpected eof, expected expr".to_string(),
                info: Info::default(),
            }
            .to_node(),
            toks,
        ))
    }
}

fn led(
    db: &dyn Compiler,
    mut toks: VecDeque<Token>,
    left: Node,
) -> Result<(Node, VecDeque<Token>), TError> {
    // eprintln!("here {:?} {:?}", toks, left);
    if let Some(Token {
        tok_type: TokenType::CloseBracket,
        pos,
        ..
    }) = toks.front()
    {
        return Ok((
            Err {
                msg: "Close bracket".to_string(),
                info: pos.clone().get_info(),
            }
            .to_node(),
            toks,
        ));
    }

    match toks.pop_front() {
        None => Ok((
            Err {
                msg: "Unexpected eof, expected expr tail".to_string(),
                info: left.get_info(),
            }
            .to_node(),
            toks,
        )),
        Some(head) => match head.tok_type {
            TokenType::NumLit | TokenType::StringLit | TokenType::Sym => {
                let pos = head.pos.clone();
                toks.push_front(head);
                toks.push_front(Token {
                    tok_type: TokenType::Op,
                    value: ";".to_string(),
                    pos,
                });
                Ok((left, toks))
            }
            TokenType::Op => {
                let lbp = binding_power(db, &head)?;
                let assoc = binding_dir(db, &head)?;
                let ( right, new_toks) = expr(
                    db,
                    toks,
                    lbp - match assoc {
                        Direction::Left => 0,
                        Direction::Right => 1,
                    },
                )?;
                if head.value != "=" {
                    return Ok((
                        bin_op(
                            head.value.as_str(),
                            left,
                            right,
                            head.get_info(),
                        ),
                        new_toks,
                    ));
                }
                match left {
                    Node::SymNode(s) => Ok((
                        Let {
                            name: s.name,
                            args: None,
                            value: Box::new(right),
                            info: head.get_info(),
                        }
                        .to_node(),
                        new_toks,
                    )),
                    Node::ApplyNode(a) => match *a.inner {
                        Node::SymNode(s) => Ok((
                            Let {
                                name: s.name,
                                args: Some(a.args.iter().map(|l| l.to_sym()).collect()),
                                value: Box::new(right),
                                info: head.get_info(),
                            }
                            .to_node(),
                            new_toks,
                        )),
                        _ => panic!(format!("Cannot assign to {}", a.to_node())),
                    },
                    _ => panic!(format!("Cannot assign to {}", left)),
                }
            }
            TokenType::CloseBracket => panic!("Unexpected close bracket"),
            TokenType::OpenBracket => {
                if head.value.as_str() == "("
                    && toks.front().map(|t| &t.value) == Some(&")".to_string())
                {
                    toks.pop_front();
                    return Ok((
                        Apply {
                            inner: Box::new(left),
                            args: vec![],
                            info: head.get_info(),
                        }
                        .to_node(),
                        toks,
                    ));
                }
                let (inner, mut new_toks) = expr(db, toks, 0)?;
                // TODO: Handle empty parens
                let close = new_toks.front();
                match (head.value.as_str(), close) {
                    (
                        open,
                        Some(Token {
                            value: close,
                            tok_type: TokenType::CloseBracket,
                            ..
                        }),
                    ) => {
                        match (open, close.as_str()) {
                            ("(", ")") => {}
                            ("[", "]") => {}
                            ("{", "}") => {}
                            (open, chr) => {
                                panic!(format!(
                                    "Unexpected closing bracket for {}, found {}.",
                                    open, chr
                                ));
                            }
                        };
                    }
                    (open, chr) => {
                        panic!("Unclosed bracket {}, found {:?}", open, chr);
                    }
                }
                new_toks.pop_front();
                // Introduce arguments
                let args = get_defs(inner);
                Ok((
                    Apply {
                        inner: Box::new(left),
                        args,
                        info: head.get_info(),
                    }
                    .to_node(),
                    new_toks,
                ))
            }
            TokenType::Unknown | TokenType::Whitespace => {
                panic!("Lexer should not produce unknown or whitespace")
            }
        },
    }
}

fn expr(
    db: &dyn Compiler,
    init_toks: VecDeque<Token>,
    init_lbp: i32,
) -> Result<(Node, VecDeque<Token>), TError> {
    // TODO: Name updates fields, this is confusing (0 is tree, 1 is toks)
    let init_update = nud(db, init_toks)?;
    let mut left: Node = init_update.0;
    let mut toks: VecDeque<Token> = init_update.1;
    loop {
        match toks.front() {
            None => break,
            Some(token) => {
                if init_lbp >= binding_power(db, token)? {
                    break;
                }
            }
        }
        let update = led(db, toks, left.clone())?;
        if let (Node::Error(_), new_toks) = update {
            return Ok((left, new_toks));
        }
        left = update.0;
        toks = update.1;
    }
    Ok((left, toks))
}

pub fn lex(db: &dyn Compiler, module: &Path) -> Result<VecDeque<Token>, TError> {
    let filename = db.filename(module.clone());
    lex_string(db, module, &db.file(filename)?.to_string())
}

pub fn lex_string(
    db: &dyn Compiler,
    module: &Path,
    contents: &str,
) -> Result<VecDeque<Token>, TError> {
    let filename = db.filename(module.clone());
    let mut toks: VecDeque<Token> = VecDeque::new();

    let mut pos = Loc {
        filename: Some(filename),
        ..Loc::default()
    };
    let mut chars = contents.chars().peekable();
    loop {
        let (next, new_chars) = lex_head(chars, &mut pos);
        if next.tok_type == TokenType::Unknown {
            break; // TODO done / skip?
        }
        // If valid, take the token and move on.
        toks.push_back(next);
        chars = new_chars;
    }
    // eprintln!("Toks: {:?}", toks);
    Ok(toks)
}

pub fn parse_string(db: &dyn Compiler, module: &Path, text: &Arc<String>) -> Result<Node, TError> {
    let toks = db.lex_string(module.to_vec(), text.clone())?;
    if db.debug() > 0 {
        eprintln!("parsing str... {:?}", &module);
    }
    let (root, left_over) = expr(db, toks, 0)?;

    if !left_over.is_empty() {
        panic!("Oh no: Left over tokens {:?}", left_over);
    }
    if db.options().show_ast {
        eprintln!("ast: {}", root);
    }
    Ok(root)
}

pub fn parse(db: &dyn Compiler, module: &Path) -> Result<Node, TError> {
    let toks = db.lex_file(module.clone())?;
    if db.debug() > 0 {
        eprintln!("parsing file... {:?}", &module);
    }
    let (root, left_over) = expr(db, toks, 0)?;

    if !left_over.is_empty() {
        panic!("Oh no: Left over tokens {:?}", left_over);
    }
    if db.options().show_ast {
        eprintln!("ast: {}", root);
    }
    Ok(root)
}

fn bin_op(name: &str, left: Node, right: Node, info: Info) -> Node {
    Apply {
        inner: Box::new(
            Sym {
                name: name.to_string(),
                info: info.clone(),
            }
            .to_node(),
        ),
        args: vec![
            Let {
                name: "left".to_string(),
                value: Box::new(Prim::Lambda(Box::new(left)).to_node()),
                args: None,
                info: info.clone(),
            },
            Let {
                name: "right".to_string(),
                value: Box::new(Prim::Lambda(Box::new(right)).to_node()),
                args: None,
                info: info.clone(),
            },
        ],
        info,
    }
    .to_node()
}

fn un_op(name: &str, inner: Node, info: Info) -> Node {
    Apply {
        inner: Box::new(
            Sym {
                name: name.to_string(),
                info: info.clone(),
            }
            .to_node(),
        ),
        args: vec![Let {
            name: "it".to_string(),
            value: Box::new(inner),
            args: None,
            info: info.clone(),
        }],
        info,
    }
    .to_node()
}

#[cfg(test)]
pub mod tests {
    use super::{parse_string, bin_op, un_op};
    use crate::ast::*;
    use crate::database::Compiler;
    use crate::database::DB;
    use Prim::*;

    fn parse(contents: &str) -> Node {
        use crate::cli_options::Options;
        use std::sync::Arc;
        let mut db = DB::default();
        let filename = "test.tk";
        db.set_options(Options::default());
        let module = db.module_name(filename.to_owned());
        parse_string(&db, &module, &Arc::new(contents.to_string())).expect("failed to parse string")
    }

    fn num_lit(x: i32) -> Node {
        I32(x, Info::default()).to_node()
    }

    fn str_lit(x: &str) -> Node {
        Str(x.to_string(), Info::default()).to_node()
    }

    #[test]
    fn parse_num() {
        assert_eq!(parse("12"), I32(12, Info::default()).to_node());
    }

    #[test]
    fn parse_str() {
        assert_eq!(
            parse("\"hello world\""),
            Str("hello world".to_string(), Info::default()).to_node()
        );
    }

    #[test]
    fn parse_un_op() {
        assert_eq!(
            parse("-12"),
            un_op("-", I32(12, Info::default()).to_node(), Info::default()).to_node()
        );
    }

    #[test]
    fn parse_min_op() {
        assert_eq!(
            parse("14-12"),
            bin_op("-", num_lit(14), num_lit(12), Info::default())
        );
    }

    #[test]
    fn parse_mul_op() {
        assert_eq!(
            parse("14*12"),
            bin_op("*", num_lit(14), num_lit(12), Info::default())
        );
    }

    #[test]
    fn parse_add_mul_precedence() {
        assert_eq!(
            parse("3+2*4"),
            bin_op(
                "+",
                num_lit(3),
                bin_op("*", num_lit(2), num_lit(4), Info::default()),
                Info::default()
            )
        );
    }

    #[test]
    fn parse_mul_add_precedence() {
        assert_eq!(
            parse("3*2+4"),
            bin_op(
                "+",
                bin_op("*", num_lit(3), num_lit(2), Info::default()),
                num_lit(4),
                Info::default()
            )
        );
    }

    #[test]
    fn parse_mul_add_parens() {
        assert_eq!(
            parse("3*(2+4)"),
            bin_op(
                "*",
                num_lit(3),
                bin_op("+", num_lit(2), num_lit(4), Info::default()),
                Info::default()
            )
        );
    }

    #[test]
    fn parse_add_str() {
        assert_eq!(
            parse("\"hello\"+\" world\""),
            bin_op("+", str_lit("hello"), str_lit(" world"), Info::default())
        );
    }

    #[test]
    fn parse_strings_followed_by_raw_values() {
        assert_eq!(
            parse("\"hello world\"\n7"),
            bin_op(
                ";",
                str_lit("hello world"),
                num_lit(7),
                Info::default()
            )
        );
    }

    #[test]
    fn parse_strings_with_operators_and_trailing_values_in_let() {
        assert_eq!(
            parse("x()= !\"hello world\"\n7"),
            bin_op(
                ";",
                Let {
                    name: "x".to_string(),
                    args: Some(vec![]),
                    value: Box::new(un_op("!", str_lit("hello world"), Info::default())),
                    info: Info::default(),
                }
                .to_node(),
                num_lit(7),
                Info::default()
            )
        );
    }
}
