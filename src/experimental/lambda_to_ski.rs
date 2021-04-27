use ski::SVal;

use crate::experimental::lambda;
use crate::experimental::ski;

use ski::SVal::*;
use ski::SKI::*;

impl lambda::Term {
    pub fn to_ski(&self) -> ski::SVal {
        todo!();
    }
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use super::*;
    use lambda::util::*;

    #[test]
    fn church_0_to_ski() {
        assert_eq!(
            church_nat(0).to_ski(),
            P(vec![T(K),T(I)].into())
        );
    }

    #[test]
    fn church_1_to_ski() {
        todo!();
    }

    #[test]
    fn church_2_to_ski() {
        todo!();
    }

    #[test]
    fn church_plus_to_ski() {
        todo!();
    }

    #[test]
    fn church_3_plus_4_to_ski() {
        todo!();
    }
}
