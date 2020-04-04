use super::ast::*;
use super::cli_options::Options;
use std::fmt::Write;

#[derive(Debug, PartialEq)]
pub enum PrettyPrintError {
    FailedParse(String, Info),
}

// Walks the AST interpreting it.
pub struct PrettyPrint {}

// TODO: Return nodes.
type Res = Result<(), PrettyPrintError>;
type State = String;
impl Visitor<State, (), String, PrettyPrintError> for PrettyPrint {
    fn new(_opts: &Options) -> PrettyPrint {
        PrettyPrint {}
    }

    fn visit_root(&mut self, expr: &Root) -> Result<String, PrettyPrintError> {
        let mut state: String = "".to_string();
        self.visit(&mut state, &expr.ast)?;
        Ok(state)
    }

    fn visit_sym(&mut self, state: &mut State, expr: &Sym) -> Res {
        write!(state, "{}", expr.name).unwrap();
        Ok(())
    }

    fn visit_prim(&mut self, state: &mut State, expr: &Prim) -> Res {
        use Prim::*;
        match expr {
            Bool(val, _) => {
                write!(state, "{}", val).unwrap();
                Ok(())
            }
            I32(val, _) => {
                write!(state, "{}", val).unwrap();
                Ok(())
            }
            Str(val, _) => {
                write!(state, "'{}'", val).unwrap();
                Ok(())
            }
            Lambda(val) => {
                self.visit(state, val)?;
                Ok(())
            }
        }
    }

    fn visit_apply(&mut self, state: &mut State, expr: &Apply) -> Res {
        self.visit(state, &*expr.inner)?;
        write!(state, "(").unwrap();
        let mut is_first = true;
        for arg in expr.args.iter() {
            if !is_first {
                write!(state, ", ").unwrap();
            }
            self.visit_let(state, &arg)?;
            is_first = false;
        }
        write!(state, ")").unwrap();
        Ok(())
    }

    fn visit_let(&mut self, state: &mut State, expr: &Let) -> Res {
        write!(state, "{}", expr.name).unwrap();
        match &expr.args {
            Some(reqs) => {
                if !reqs.is_empty() {
                    write!(state, "(").unwrap();
                }
                let mut is_first = true;
                for arg in reqs {
                    if !is_first {
                        write!(state, ", ").unwrap();
                    }
                    self.visit_sym(state, &arg)?;
                    is_first = false;
                }
                if !reqs.is_empty() {
                    write!(state, ")").unwrap();
                }
            }
            None => {}
        }
        write!(state, "=").unwrap();
        self.visit(state, &*expr.value)
    }

    fn visit_un_op(&mut self, state: &mut State, expr: &UnOp) -> Res {
        write!(state, "({}", expr.name).unwrap();
        self.visit(state, &*expr.inner)?;
        write!(state, ")").unwrap();
        Ok(())
    }

    fn visit_bin_op(&mut self, state: &mut State, expr: &BinOp) -> Res {
        write!(state, "(").unwrap();
        self.visit(state, &*expr.left)?;
        write!(state, "{}", expr.name).unwrap();
        self.visit(state, &*expr.right)?;
        write!(state, ")").unwrap();
        Ok(())
    }

    fn handle_error(&mut self, _state: &mut State, expr: &Err) -> Res {
        Err(PrettyPrintError::FailedParse(
            expr.msg.to_string(),
            expr.get_info(),
        ))
    }
}

#[cfg(test)]
mod tests {}
