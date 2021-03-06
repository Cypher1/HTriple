use std::collections::HashSet;
use std::fmt;
use std::hash::{Hash, Hasher};

use crate::database::DBStorage;
use crate::errors::TError;
use crate::location::*;
use crate::primitives::{unit_type, Val};
use crate::symbol_table::Table;

impl HasInfo for TError {
    fn get_info(&self) -> Info {
        use TError::*;
        match self {
            CppCompilerError(_, _, info) => info.clone(),
            UnknownSymbol(_, info, _) => info.clone(),
            OutOfScopeTypeVariable(_, info) => info.clone(),
            UnknownInfixOperator(_, info) => info.clone(),
            UnknownPrefixOperator(_, info) => info.clone(),
            UnknownSizeOfVariableType(_, info) => info.clone(),
            UnknownSizeOfAbstractType(_, info) => info.clone(),
            UnknownCardOfAbstractType(_, info) => info.clone(),
            StaticPointerCardinality(info) => info.clone(),
            TypeMismatch(_, _, info) => info.clone(),
            TypeMismatch2(_, _, _, info) => info.clone(),
            RequirementFailure(info) => info.clone(),
            ParseError(_, info) => info.clone(),
            InternalError(_, info) => info.clone(),
            ExpectedLetNode(node) => node.get_info(),
            UnknownPath(_, info) => info.clone(),
        }
    }
    fn get_mut_info(&mut self) -> &mut Info {
        use TError::*;
        match self {
            CppCompilerError(_, _, ref mut info) => info,
            UnknownSymbol(_, ref mut info, _) => info,
            OutOfScopeTypeVariable(_, ref mut info) => info,
            UnknownInfixOperator(_, ref mut info) => info,
            UnknownPrefixOperator(_, ref mut info) => info,
            UnknownSizeOfVariableType(_, ref mut info) => info,
            UnknownSizeOfAbstractType(_, ref mut info) => info,
            UnknownCardOfAbstractType(_, ref mut info) => info,
            StaticPointerCardinality(ref mut info) => info,
            TypeMismatch(_, _, ref mut info) => info,
            TypeMismatch2(_, _, _, ref mut info) => info,
            RequirementFailure(ref mut info) => info,
            ParseError(_, ref mut info) => info,
            InternalError(_, ref mut info) => info,
            ExpectedLetNode(ref mut node) => node.get_mut_info(),
            UnknownPath(_, ref mut info) => info,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub struct Apply {
    pub inner: Box<Node>,
    pub args: Vec<Let>,
    pub info: Info,
}

impl ToNode for Apply {
    fn into_node(self) -> Node {
        Node::ApplyNode(self)
    }
}
impl HasInfo for Apply {
    fn get_info(&self) -> Info {
        self.info.clone()
    }
    fn get_mut_info(&mut self) -> &mut Info {
        &mut self.info
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, PartialOrd, Ord)]
pub struct Sym {
    pub name: String,
    pub info: Info,
}

impl Sym {
    pub fn as_let(self: &Sym) -> Let {
        Let {
            name: self.name.clone(),
            value: Box::new(unit_type().into_node()),
            args: None,
            info: self.get_info(),
        }
    }
}

impl ToNode for Sym {
    fn into_node(self) -> Node {
        Node::SymNode(self)
    }
}
impl HasInfo for Sym {
    fn get_info(&self) -> Info {
        self.info.clone()
    }
    fn get_mut_info(&mut self) -> &mut Info {
        &mut self.info
    }
}

impl ToNode for Val {
    fn into_node(self) -> Node {
        Node::ValNode(self, Info::default())
    }
}

// Consider finding way to turn lets into binary operators.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct Abs {
    pub name: String,
    pub value: Box<Node>,
    pub info: Info,
}

impl Abs {
    pub fn to_sym(self: &Abs) -> Sym {
        Sym {
            name: self.name.clone(),
            info: self.get_info(),
        }
    }
}

impl ToNode for Abs {
    fn into_node(self) -> Node {
        Node::AbsNode(self)
    }
}
impl HasInfo for Abs {
    fn get_info(&self) -> Info {
        self.info.clone()
    }
    fn get_mut_info(&mut self) -> &mut Info {
        &mut self.info
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct Let {
    pub name: String,
    pub value: Box<Node>,
    pub args: Option<Vec<Let>>,
    pub info: Info,
    // TODO; support captures
}

impl Let {
    pub fn to_sym(self: &Let) -> Sym {
        Sym {
            name: self.name.clone(),
            info: self.get_info(),
        }
    }
    pub fn new(name: &str, value: Val) -> Let {
        Let {
            name: name.to_string(),
            value: Box::new(value.into_node()),
            args: Some(vec![]),
            info: Info::default(),
        }
    }
}

impl ToNode for Let {
    fn into_node(self) -> Node {
        Node::LetNode(self)
    }
}
impl HasInfo for Let {
    fn get_info(&self) -> Info {
        self.info.clone()
    }
    fn get_mut_info(&mut self) -> &mut Info {
        &mut self.info
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct UnOp {
    pub name: String,
    pub inner: Box<Node>,
    pub info: Info,
}

impl ToNode for UnOp {
    fn into_node(self) -> Node {
        Node::UnOpNode(self)
    }
}
impl HasInfo for UnOp {
    fn get_info(&self) -> Info {
        self.info.clone()
    }
    fn get_mut_info(&mut self) -> &mut Info {
        &mut self.info
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct BinOp {
    pub name: String,
    pub left: Box<Node>,
    pub right: Box<Node>,
    pub info: Info,
}

impl ToNode for BinOp {
    fn into_node(self) -> Node {
        Node::BinOpNode(self)
    }
}
impl HasInfo for BinOp {
    fn get_info(&self) -> Info {
        self.info.clone()
    }
    fn get_mut_info(&mut self) -> &mut Info {
        &mut self.info
    }
}

#[derive(Clone, PartialOrd, Ord)]
pub struct Info {
    pub loc: Option<Loc>,
    pub ty: Option<Box<Node>>,
    pub defined_at: Option<Path>,
    pub callable: bool,
}

impl Default for Info {
    fn default() -> Info {
        Info {
            loc: None,
            ty: None,
            defined_at: None,
            callable: false,
        }
    }
}

impl std::fmt::Debug for Info {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(loc) = &self.loc {
            write!(f, "{:?}", loc)?;
        }
        if let Some(ty) = &self.ty {
            write!(f, "{:?}", ty)?;
        }
        if let Some(def) = &self.defined_at {
            write!(f, " from {:?}", def)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Info {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Eq for Info {}
impl PartialEq for Info {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl Hash for Info {
    fn hash<H: Hasher>(&self, _state: &mut H) {}
}

// #[derive(Debug)]
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum Node {
    SymNode(Sym),
    ValNode(Val, Info),
    ApplyNode(Apply),
    AbsNode(Abs),
    LetNode(Let),
    UnOpNode(UnOp),
    BinOpNode(BinOp),
}

impl std::fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Node::*;
        match self {
            SymNode(n) => n.fmt(f),
            ValNode(n, _) => n.fmt(f),
            ApplyNode(n) => n.fmt(f),
            AbsNode(n) => n.fmt(f),
            LetNode(n) => n.fmt(f),
            UnOpNode(n) => n.fmt(f),
            BinOpNode(n) => n.fmt(f),
        }
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use super::PrettyPrint;
        // TODO worry about unsafe default here
        match PrettyPrint::default().visit_root(&mut DBStorage::default(), self) {
            Ok(res) => write!(f, "{}", res),
            Err(err) => write!(f, "{:#?}", err),
        }
    }
}

impl ToNode for Node {
    fn into_node(self) -> Node {
        self
    }
}
impl HasInfo for Node {
    fn get_info(&self) -> Info {
        use Node::*;
        match self {
            SymNode(n) => n.get_info(),
            ValNode(_n, info) => info.clone(),
            ApplyNode(n) => n.get_info(),
            AbsNode(n) => n.get_info(),
            LetNode(n) => n.get_info(),
            UnOpNode(n) => n.get_info(),
            BinOpNode(n) => n.get_info(),
        }
    }
    fn get_mut_info(&mut self) -> &mut Info {
        use Node::*;
        match self {
            SymNode(ref mut n) => n.get_mut_info(),
            ValNode(_, ref mut info) => info,
            ApplyNode(ref mut n) => n.get_mut_info(),
            AbsNode(ref mut n) => n.get_mut_info(),
            LetNode(ref mut n) => n.get_mut_info(),
            UnOpNode(ref mut n) => n.get_mut_info(),
            BinOpNode(ref mut n) => n.get_mut_info(),
        }
    }
}

impl Node {
    pub fn as_let(&self) -> Result<Let, TError> {
        use Node::*;
        if let LetNode(n) = self {
            return Ok(n.clone());
        }
        Err(TError::ExpectedLetNode(Box::new(self.clone())))
    }
}

pub trait ToNode {
    fn into_node(self) -> Node;
}

pub trait HasInfo {
    fn get_info(&self) -> Info;
    fn get_mut_info(&mut self) -> &mut Info;
}

#[derive(Debug, Clone, Ord, PartialOrd, Hash, PartialEq, Eq)]
pub enum Symbol {
    Anon(),
    Named(String, Option<String>), // name, (and for files) an optional extension
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Symbol::Anon() => write!(f, "_")?,
            // TODO: Edge case exists here if two files with different extensions are used together
            Symbol::Named(name, _) => write!(f, "{}", name)?,
        }
        Ok(())
    }
}

impl Symbol {
    pub fn new(name: &str) -> Symbol {
        Symbol::Named(name.to_string(), None)
    }
    pub fn to_name(self: &Symbol) -> String {
        format!("{}", &self)
    }
    pub fn to_filename(self: &Symbol) -> String {
        match self {
            Symbol::Anon() => "".to_owned(),
            // TODO: Edge case exists here if two files with different extensions are used together
            Symbol::Named(name, ext) => format!(
                "{}{}",
                name,
                ext.as_ref()
                    .map(|v| format!(".{}", v))
                    .unwrap_or_else(|| "".to_string())
            ),
        }
    }
}

pub type Path = Vec<Symbol>;
pub type PathRef<'a> = &'a [Symbol];

pub fn path_to_string(path: PathRef) -> String {
    path.iter()
        .map(|p| format!("{}", p))
        .collect::<Vec<String>>()
        .join(".")
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Entry {
    pub uses: HashSet<Path>,
    pub defined_at: Path,
    // pub requires: Vec<Sym>,
    // pub defines: HashMap<Sym, Path>,
}

impl Default for Entry {
    fn default() -> Entry {
        Entry {
            uses: HashSet::new(),
            defined_at: vec![], //TODO: Remove the default instance.
                                // requires: vec![],
                                // defines: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Root {
    pub ast: Node,
    pub table: Table,
}

impl fmt::Display for Root {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.ast.fmt(f)?;
        write!(f, "{:?}", self.table)?;
        Ok(())
    }
}

pub trait Visitor<State, Res, Final, Start = Root> {
    fn visit_root(&mut self, storage: &mut DBStorage, e: &Start) -> Result<Final, TError>;

    fn visit_sym(
        &mut self,
        storage: &mut DBStorage,
        state: &mut State,
        e: &Sym,
    ) -> Result<Res, TError>;
    fn visit_val(
        &mut self,
        storage: &mut DBStorage,
        state: &mut State,
        e: &Val,
    ) -> Result<Res, TError>;
    fn visit_apply(
        &mut self,

        storage: &mut DBStorage,
        state: &mut State,
        e: &Apply,
    ) -> Result<Res, TError>;
    fn visit_abs(
        &mut self,
        storage: &mut DBStorage,
        state: &mut State,
        e: &Abs,
    ) -> Result<Res, TError>;
    fn visit_let(
        &mut self,
        storage: &mut DBStorage,
        state: &mut State,
        e: &Let,
    ) -> Result<Res, TError>;
    fn visit_un_op(
        &mut self,

        storage: &mut DBStorage,
        state: &mut State,
        e: &UnOp,
    ) -> Result<Res, TError>;
    fn visit_bin_op(
        &mut self,

        storage: &mut DBStorage,
        state: &mut State,
        e: &BinOp,
    ) -> Result<Res, TError>;

    fn visit(
        &mut self,
        storage: &mut DBStorage,
        state: &mut State,
        e: &Node,
    ) -> Result<Res, TError> {
        use Node::*;
        match e {
            SymNode(n) => self.visit_sym(storage, state, n),
            ValNode(n, _) => self.visit_val(storage, state, n),
            ApplyNode(n) => self.visit_apply(storage, state, n),
            AbsNode(n) => self.visit_abs(storage, state, n),
            LetNode(n) => self.visit_let(storage, state, n),
            UnOpNode(n) => self.visit_un_op(storage, state, n),
            BinOpNode(n) => self.visit_bin_op(storage, state, n),
        }
    }

    fn process(root: &Start, storage: &mut DBStorage) -> Result<Final, TError>
    where
        Self: Sized,
        Self: Default,
    {
        Self::default().visit_root(storage, root)
    }
}
