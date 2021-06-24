type ID = usize;
use thiserror::Error;

use derivative::Derivative;
#[derive(Error, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Derivative)]
#[derivative(Debug)]
pub enum GraphError {
    #[error("node {0} not found in graph")]
    NodeNotFound(ID),
}
use GraphError::*;

struct GraphNode<T> {
    value: T,
    children: Vec<ID>,
}

impl<T> GraphNode<T> {
    fn new(value: T) -> Self {
        Self {
            value,
            children: vec![],
        }
    }
}

/* Technically this is not just a graph, it also supports:
 * - multiple edges between the same two nodes
 * - ordering of edges (allowing tree style structures)
*/

pub struct ArenaGraph<T> {
    values: Vec<GraphNode<T>>,
}

impl<T>  ArenaGraph<T> {
    pub fn default() -> Self {
        Self {
            values: vec![],
        }
    }

    pub fn alloc(self: &mut Self, value: T) -> ID {
        let id = self.values.len();
        self.values.push(GraphNode::new(value));
        id
    }

    pub fn add_edge(self: &mut Self, from: ID, to: ID) -> Result<(), GraphError> {
        if let Some(node) = &mut self.values.get_mut(from) {
            node.children.push(to);
        }
        Ok(())
    }

    pub fn size(self: &Self) -> usize {
        self.values.len()
    }

    fn get_node(self: &Self, id: ID) -> Result<&GraphNode<T>, GraphError> {
        self.values.get(id).map(|node| Ok(node)).unwrap_or_else(|| Err(NodeNotFound(id)))
    }

    fn get_node_mut(self: &mut Self, id: ID) -> Result<&mut GraphNode<T>, GraphError> {
        self.values.get_mut(id).map(|node| Ok(node)).unwrap_or_else(|| Err(NodeNotFound(id)))
    }

    pub fn get_value(self: &Self, id: ID) -> Result<&T, GraphError> {
        self.get_node(id).map(|node| &node.value)
    }

    pub fn get_value_mut(self: &mut Self, id: ID) -> Result<&mut T, GraphError> {
        self.get_node_mut(id).map(|node| &mut node.value)
    }

    fn get_children(self: &Self, id: ID) -> Result<&Vec<ID>, GraphError> {
        self.get_node(id).map(|node| &node.children)
    }

    fn get_children_mut(self: &mut Self, id: ID) -> Result<&mut Vec<ID>, GraphError> {
        self.get_node_mut(id).map(|node| &mut node.children)
    }

    pub fn visit<R>(self: &mut Self, id: ID, f: &mut dyn FnMut(&mut T, &mut Vec<ID>)->R) -> Result<R, GraphError> {
        let (val, children) = {
            let node = self.get_node_mut(id)?;
            (&mut node.value, &mut node.children)
        };
        Ok(f(val, children))
    }

    pub fn visit_all<R>(self: &mut Self, f: &mut dyn FnMut(&mut T, &mut Vec<ID>)->R) -> Result<Vec<R>, GraphError> {
        Ok(self.values.iter_mut().map(|node| f(&mut node.value, &mut node.children)).collect())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    type Test = Result<(), GraphError>;

    #[test]
    fn single_node_retrieval() -> Test {
        let mut g: ArenaGraph<i32> = ArenaGraph::default();

        let a = g.alloc(3);
        assert_eq!(g.get_value(a), Ok(&3), "Graph node info should be accessible");
        Ok(())
    }

    #[test]
    fn self_reference() -> Test {
        let mut g: ArenaGraph<i32> = ArenaGraph::default();

        let a = g.alloc(3);

        g.add_edge(a, a)?;

        assert_eq!(g.get_children(a), Ok(&vec![a]), "Children of a should be [a]");
        Ok(())
    }

    #[test]
    fn build_a_cycle() -> Test {
        let mut g: ArenaGraph<i32> = ArenaGraph::default();

        let a = g.alloc(3);
        let b = g.alloc(4);

        g.add_edge(a, b)?;
        g.add_edge(b, a)?;

        assert_eq!(g.get_children(a)?, &vec![b], "Children of a should be [b]");
        assert_eq!(g.get_children(b)?, &vec![a], "Children of b should be [a]");
        Ok(())
    }

    #[test]
    fn visit_a_node() -> Test {
        let mut g: ArenaGraph<i32> = ArenaGraph::default();

        let a = g.alloc(3);
        let b = g.alloc(4);

        g.add_edge(a, b)?;
        g.add_edge(b, a)?;

        assert_eq!(g.visit(a, &mut |val, children| (val.clone(), children.clone()))?, (3, vec![b]));
        Ok(())
    }


    #[test]
    fn modify_node() -> Test {
        let mut g: ArenaGraph<i32> = ArenaGraph::default();

        let a = g.alloc(3);
        let b = g.alloc(4);

        g.add_edge(a, b)?;
        g.add_edge(b, a)?;

        assert_eq!(g.visit(a, &mut |val, _children| {*val *= 2; ()})?, ());

        assert_eq!(g.visit(a, &mut |val, children| (val.clone(), children.clone()))?, (6, vec![b]));
        assert_eq!(g.visit(b, &mut |val, children| (val.clone(), children.clone()))?, (4, vec![a]));
        Ok(())
    }


    #[test]
    fn modify_nodes() -> Test {
        let mut g: ArenaGraph<i32> = ArenaGraph::default();

        let a = g.alloc(3);
        let b = g.alloc(4);

        g.add_edge(a, b)?;
        g.add_edge(b, a)?;

        assert_eq!(g.visit_all(&mut |val, _children| {*val *= 2; ()})?, vec![(), ()]);

        assert_eq!(g.visit(a, &mut |val, children| (val.clone(), children.clone()))?, (6, vec![b]));
        assert_eq!(g.visit(b, &mut |val, children| (val.clone(), children.clone()))?, (8, vec![a]));
        Ok(())
    }


    #[test]
    fn follow_cycle() -> Test {
        let mut g: ArenaGraph<i32> = ArenaGraph::default();
        let a = g.alloc(3);
        let b = g.alloc(4);

        g.add_edge(a, b)?;
        g.add_edge(b, a)?;

        let mut curr = a;
        for _ in 0..1000 {
            println!("At node: {}", &curr);
            curr = *g.get_children(curr)?.first().expect("Shouldn't run out of nodes");
        }
        Ok(())
    }
}
