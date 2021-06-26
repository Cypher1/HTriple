use specs::prelude::*;
use specs::Component;
use component_group::ComponentGroup;

use crate::ast::{Node, Path};
use crate::errors::TError;
use crate::location::Loc;
use crate::primitives::Val;
use crate::tokens::TokenType;

#[derive(Component, Clone, Debug)]
#[storage(VecStorage)]
pub struct AtLoc {
    pub loc: Loc,
}

#[derive(Component, Clone, Debug)]
#[storage(VecStorage)]
pub struct ParsedAst {
    pub ast: Node,
}

#[derive(Component, Clone, Debug)]
#[storage(VecStorage)]
pub struct AtPath {
    pub path: Path,
}

#[derive(Component, Default, Debug)]
#[storage(NullStorage)]
pub struct Untyped;

#[derive(Component, Clone, Debug)]
#[storage(VecStorage)]
pub struct Typed {
    ty: Val,
}

#[derive(Component, Clone, Debug)]
#[storage(VecStorage)]
pub struct Token {
    pub token: TokenType,
    pub value: String,
}

#[derive(Component, Clone, Debug)]
#[storage(VecStorage)]
pub struct HasErrors {
    pub error: Vec<TError>,
}

#[derive(Component, Clone, Debug)]
#[storage(VecStorage)]
pub struct HasValue {
    pub value: Val,
}

#[derive(Component, Clone, Default, Debug)]
#[storage(NullStorage)]
pub struct IsSymbol;

#[derive(Component, Clone, Default, Debug)]
#[storage(NullStorage)]
pub struct IsAbstraction;

#[derive(Component, Clone, Default, Debug)]
#[storage(NullStorage)]
pub struct IsDefinition;

// This is all of the code you need to write to define the group and all of its operations!
#[derive(ComponentGroup)]
struct Definition {
    path: AtPath,
    location: AtLoc,
    ast: ParsedAst,
    is_definition: IsDefinition,
}
