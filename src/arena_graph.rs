use thiserror::Error;
use std::{fmt, fmt::{Display, Debug}};
use derivative::Derivative;

pub type ID = usize;
#[derive(Error, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Derivative)]
#[derivative(Debug)]
pub enum GraphError {
    #[error("node {0} not found in graph")]
    NodeNotFound(ID),
}
use GraphError::*;

#[derive(Clone)]
struct GraphNode<T> {
    value: T,
    children: Vec<ID>,
}

impl <T: Debug> Debug for GraphNode<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.children.is_empty() {
            write!(f, "{:?}", self.value)
        } else {
            write!(f, "{:?}({:?})", self.value, self.children)
        }
    }
}

impl <T: Display> Display for GraphNode<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)?;
        if !self.children.is_empty() {
            write!(f, "(")?;
            for child in self.children.iter() {
                write!(f, "{}", child)?;
            }
            write!(f, ")")?;
        }
        Ok(())
    }
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

#[derive(Clone, Debug)]
pub struct ArenaGraph<T> {
    values: Vec<GraphNode<T>>,
}

impl<T>  ArenaGraph<T> {
    pub fn default() -> Self {
        Self {
            values: vec![],
        }
    }
}

pub struct NodeRef<'a, T> {
    graph: &'a ArenaGraph<T>,
    id: ID,
}

impl <'a, T> NodeRef<'a, T> {
    fn new (graph: &'a ArenaGraph<T>, id: ID) -> Self {
        Self { graph, id }
    }
    fn deref(self: &Self) -> Result<&GraphNode<T>, GraphError> {
        self.graph.values.get(self.id).map(|node| Ok(node)).unwrap_or_else(|| Err(NodeNotFound(self.id)))
    }
    fn get_value(self: &Self) -> Result<&T, GraphError> {
        self.deref().map(|node| &node.value)
    }
    fn children(self: &Self) -> Result<&Vec<ID>, GraphError> {
        self.deref().map(|node| &node.children)
    }
    pub fn visit<R>(self: &Self, f: &mut dyn FnMut(&T, &Vec<ID>)->R) -> Result<R, GraphError> {
        let node = self.deref()?;
        Ok(f(&node.value, &node.children))
    }
}

pub struct NodeMutRef<'a, T> {
    graph: &'a mut ArenaGraph<T>,
    id: ID,
}

impl <'a, T> NodeMutRef<'a, T> {
    fn new (graph: &'a mut ArenaGraph<T>, id: ID) -> Self {
        Self { graph, id }
    }
    fn deref(self: &mut Self) -> Result<&mut GraphNode<T>, GraphError> {
        let id = self.id;
        self.graph.values.get_mut(self.id).map(|node| Ok(node)).unwrap_or_else(|| Err(NodeNotFound(id)))
    }
    pub fn get_value(self: &mut Self) -> Result<&mut T, GraphError> {
        self.deref().map(|node| &mut node.value)
    }
    fn children(self: &mut Self) -> Result<&mut Vec<ID>, GraphError> {
        self.deref().map(|node| &mut node.children)
    }
    pub fn visit<R>(self: &mut Self, f: &mut dyn FnMut(&mut T, &mut Vec<ID>)->R) -> Result<R, GraphError> {
        let node = self.deref()?;
        Ok(f(&mut node.value, &mut node.children))
    }
}

impl<T: Debug + Display>  ArenaGraph<T> {
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

    fn get_node(self: &Self, id: ID) -> Result<NodeRef<T>, GraphError> {
        let r = NodeRef::new(self, id);
        r.deref()?;
        Ok(r)
    }

    fn get_node_mut(self: &mut Self, id: ID) -> Result<NodeMutRef<T>, GraphError> {
        let mut r = NodeMutRef::new(self, id);
        r.deref()?;
        Ok(r)
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
        assert_eq!(g.get_node(a)?.get_value(), Ok(&3), "Graph node info should be accessible");
        Ok(())
    }

    #[test]
    fn self_reference() -> Test {
        let mut g: ArenaGraph<i32> = ArenaGraph::default();

        let a = g.alloc(3);

        g.add_edge(a, a)?;

        assert_eq!(g.get_node(a)?.children(), Ok(&vec![a]), "Children of a should be [a]");
        Ok(())
    }

    #[test]
    fn build_a_cycle() -> Test {
        let mut g: ArenaGraph<i32> = ArenaGraph::default();

        let a = g.alloc(3);
        let b = g.alloc(4);

        g.add_edge(a, b)?;
        g.add_edge(b, a)?;

        assert_eq!(g.get_node(a)?.children()?, &vec![b], "Children of a should be [b]");
        assert_eq!(g.get_node(b)?.children()?, &vec![a], "Children of b should be [a]");
        Ok(())
    }

    #[test]
    fn visit_a_node() -> Test {
        let mut g: ArenaGraph<i32> = ArenaGraph::default();

        let a = g.alloc(3);
        let b = g.alloc(4);

        g.add_edge(a, b)?;
        g.add_edge(b, a)?;

        assert_eq!(g.get_node(a)?.visit(&mut |val, children| (val.clone(), children.clone()))?, (3, vec![b]));
        Ok(())
    }


    #[test]
    fn modify_node() -> Test {
        let mut g: ArenaGraph<i32> = ArenaGraph::default();

        let a = g.alloc(3);
        let b = g.alloc(4);

        g.add_edge(a, b)?;
        g.add_edge(b, a)?;

        assert_eq!(g.get_node_mut(a)?.visit(&mut |val, _children| {*val *= 2; ()})?, ());

        assert_eq!(g.get_node(a)?.visit(&mut |val, children| (val.clone(), children.clone()))?, (6, vec![b]));
        assert_eq!(g.get_node(b)?.visit(&mut |val, children| (val.clone(), children.clone()))?, (4, vec![a]));
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

        assert_eq!(g.get_node(a)?.visit(&mut |val, children| (val.clone(), children.clone()))?, (6, vec![b]));
        assert_eq!(g.get_node(b)?.visit(&mut |val, children| (val.clone(), children.clone()))?, (8, vec![a]));
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
            curr = *g.get_node(curr)?.children()?.first().expect("Shouldn't run out of nodes");
        }
        Ok(())
    }
}
