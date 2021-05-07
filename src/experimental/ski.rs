#![allow(dead_code)]
use std::collections::VecDeque;

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub enum SKI {
    S,
    K,
    I,
    V(String),
    P(Stack),
}

type Stack = VecDeque<SKI>;

use SKI::*;

pub fn eval(mut stack: Stack) -> Stack {
    // eprintln!("{:?}", shows(&stack));
    while let Some(curr) = stack.pop_front() {
        match curr {
            S => {
                if stack.len() >= 3 {
                    let x = stack
                        .pop_front()
                        .expect("Empty pop_front from non-empty stack (S.x).");
                    let y = stack
                        .pop_front()
                        .expect("Empty pop_front from non-empty stack (S.y).");
                    let z = stack
                        .pop_front()
                        .expect("Empty pop_front from non-empty stack (S.z).");
                    // xz(yz)
                    let mut parend = vec![];
                    if let P(y) = y {
                        parend.extend(y);
                    } else {
                        parend.push(y);
                    }
                    parend.push(z.clone());
                    stack.push_front(P(parend.into()));
                    stack.push_front(z);
                    stack.push_front(x);
                } else {
                    stack.push_front(S);
                    return stack;
                }
            }
            K => {
                if stack.len() >= 2 {
                    let val = stack
                        .pop_front()
                        .expect("Empty pop_front from non-empty stack (K).");
                    stack.pop_front(); // drop
                    stack.push_front(val); // TODO: Replace rather than pop_front+push_front
                } else {
                    stack.push_front(K);
                    return stack;
                }
            }
            I => {} // Identity
            V(name) => {
                let mut simplified = VecDeque::new();
                for val in stack.iter().cloned() {
                    let out = if let P(val) = val {
                        let mut vals = eval(val);
                        if vals.len() == 1 {
                            vals.pop_front().expect("Length 1 vec should have a value")
                        } else {
                            P(vals)
                        }
                    } else {
                        val
                    };
                    simplified.push_back(out);
                }
                simplified.push_front(V(name));
                return simplified;
            }
            P(vs) => {
                for v in vs.iter().rev() {
                    stack.push_front(v.clone());
                }
            }
        }
        // eprintln!("{:?}", shows(&stack));
    }
    // Error: no instructions
    panic!("no instructions")
}

pub fn show(s: &SKI) -> String {
    match s {
        S => "S".to_string(),
        K => "K".to_string(),
        I => "I".to_string(),
        V(name) => name.to_string(),
        P(st) => {
            let mut s = "(".to_string();
            for t in st.iter() {
                s += &show(t);
            }
            s += ")";
            s
        }
    }
}

pub fn shows(s: &Stack) -> String {
    show(&P(s.clone()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn v(name: &str) -> SKI {
        SKI::V(name.to_string())
    }

    fn test(stack: Stack, expected: Stack) {
        eprintln!("Running: {:?}", &stack);
        let out = eval(stack);
        eprintln!("Got: {:?}", &out);

        assert_eq!(out, expected);
    }

    #[test]
    fn term_i() {
        test(
            vec![I, v("x"), v("y"), v("z")].into(),
            vec![v("x"), v("y"), v("z")].into(),
        );
    }

    #[test]
    fn term_k() {
        test(
            vec![K, v("x"), v("y"), v("z")].into(),
            vec![v("x"), v("z")].into(),
        );
    }

    #[test]
    fn term_s() {
        test(
            vec![S, v("x"), v("y"), v("z")].into(),
            vec![v("x"), v("z"), P(vec![v("y"), v("z")].into())].into(),
        );
    }

    #[test]
    fn skk_is_i() {
        test(
            vec![
                S,
                K,
                K,
                v("x"),
                v("y"),
                v("z"),
            ]
            .into(),
            vec![v("x"), v("y"), v("z")].into(),
        );
    }

    #[test]
    fn sks_is_i() {
        test(
            vec![
                S,
                K,
                S,
                v("x"),
                v("y"),
                v("z"),
            ]
            .into(),
            vec![v("x"), v("y"), v("z")].into(),
        );
    }

    #[test]
    fn complex_expression() {
        /*
        S(K(SI))Kαβ →
        K(SI)α(Kα)β →
        SI(Kα)β →
        Iβ(Kαβ) →
        Iβα →
        βα
         */
        test(
            vec![
                S,
                P(vec![K, P(vec![S, I].into())].into()),
                K,
                v("a"),
                v("b"),
            ]
            .into(),
            vec![v("b"), v("a")].into(),
        );
    }
}
