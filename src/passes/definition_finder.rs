use crate::ast::*;
use crate::database::DBStorage;
use crate::errors::TError;
use crate::passes::symbol_table_builder::State;
use crate::primitives::Val;

// Walks the AST interpreting it.
#[derive(Default)]
pub struct DefinitionFinder {}

// TODO: Return nodes.
type Res = Result<Node, TError>;

#[derive(Debug, Clone)]
pub struct Namespace {
    name: Symbol,
    info: Entry,
}

impl Visitor<State, Node, Root, Path> for DefinitionFinder {
    fn visit_root(&mut self, storage: &mut DBStorage, module: &Path) -> Result<Root, TError> {
        let expr = storage.build_symbol_table(module.clone())?;
        if storage.debug_level() > 0 {
            eprintln!(
                "looking up definitions in file... {}",
                path_to_string(module)
            );
        }
        let mut state = State {
            path: module.clone(),
            table: expr.table.clone(),
        };
        let ast = self.visit(storage, &mut state, &expr.ast)?;
        Ok(Root {
            ast,
            table: state.table,
        })
    }

    fn visit_sym(&mut self, storage: &mut DBStorage, state: &mut State, expr: &Sym) -> Res {
        if storage.debug_level() > 1 {
            eprintln!(
                "visiting sym {} {}",
                path_to_string(&state.path),
                &expr.name
            );
        }
        let mut search: Vec<Symbol> = state.path.clone();
        loop {
            if let Some(Symbol::Anon()) = search.last() {
                search.pop(); // Cannot look inside an 'anon'.
            }
            search.push(Symbol::new(&expr.name));
            let node = state.table.find_mut(&search);
            match node {
                Some(node) => {
                    node.value.uses.insert(state.path.clone());
                    if storage.debug_level() > 1 {
                        eprintln!(
                            "FOUND {} at {}\n",
                            expr.name.clone(),
                            path_to_string(&search)
                        );
                    }
                    let mut res = expr.clone();
                    res.info.defined_at = Some(search);
                    return Ok(res.into_node());
                }
                None => {
                    search.pop(); // Strip the name off.
                    if storage.debug_level() > 1 {
                        eprintln!(
                            "   not found {} at {}",
                            expr.name.clone(),
                            path_to_string(&search)
                        );
                    }
                    if search.is_empty() {
                        return Err(TError::UnknownSymbol(
                            expr.name.clone(),
                            expr.get_info(),
                            path_to_string(&state.path),
                        ));
                    }
                    search.pop(); // Up one, go again.
                }
            }
        }
    }

    fn visit_val(&mut self, _storage: &mut DBStorage, _state: &mut State, expr: &Val) -> Res {
        Ok(expr.clone().into_node())
    }

    fn visit_apply(&mut self, storage: &mut DBStorage, state: &mut State, expr: &Apply) -> Res {
        state.path.push(Symbol::Anon());
        let args = expr
            .args
            .iter()
            .map(|arg| {
                let val = self.visit_let(storage, state, arg)?.as_let();
                let mut search = state.path.clone();
                search.push(Symbol::new(&arg.name));
                let node = state.table.find_mut(&search);
                if let Some(node) = node {
                    node.value.uses.insert(state.path.clone());
                }
                val
            })
            .collect::<Result<Vec<Let>, TError>>()?;
        let inner = Box::new(self.visit(storage, state, &*expr.inner)?);
        state.path.pop();
        Ok(Apply {
            inner,
            args,
            info: expr.get_info(),
        }
        .into_node())
    }

    fn visit_abs(&mut self, storage: &mut DBStorage, state: &mut State, expr: &Abs) -> Res {
        if storage.debug_level() > 1 {
            eprintln!("visiting {} {}", path_to_string(&state.path), &expr.name);
        }
        let value = Box::new(self.visit(storage, state, &expr.value)?);
        Ok(Abs {
            name: expr.name.clone(),
            value,
            info: expr.info.clone(),
        }
        .into_node())
    }

    fn visit_let(&mut self, storage: &mut DBStorage, state: &mut State, expr: &Let) -> Res {
        if storage.debug_level() > 1 {
            eprintln!("visiting {} {}", path_to_string(&state.path), &expr.name);
        }
        let path_name = Symbol::new(&expr.name);
        state.path.push(path_name);
        let args = if let Some(args) = &expr.args {
            Some(
                args.iter()
                    .map(|arg| self.visit_let(storage, state, arg)?.as_let())
                    .collect::<Result<Vec<Let>, TError>>()?,
            )
        } else {
            None
        };
        let value = Box::new(self.visit(storage, state, &expr.value)?);
        state.path.pop();
        Ok(Let {
            name: expr.name.clone(),
            args,
            value,
            info: expr.info.clone(),
        }
        .into_node())
    }

    fn visit_un_op(&mut self, storage: &mut DBStorage, state: &mut State, expr: &UnOp) -> Res {
        let inner = Box::new(self.visit(storage, state, &expr.inner)?);
        Ok(UnOp {
            name: expr.name.clone(),
            inner,
            info: expr.get_info(),
        }
        .into_node())
    }

    fn visit_bin_op(&mut self, storage: &mut DBStorage, state: &mut State, expr: &BinOp) -> Res {
        let left = Box::new(self.visit(storage, state, &expr.left)?);
        let right = Box::new(self.visit(storage, state, &expr.right)?);
        Ok(BinOp {
            name: expr.name.clone(),
            left,
            right,
            info: expr.get_info(),
        }
        .into_node())
    }
}

#[cfg(test)]
mod tests {}
