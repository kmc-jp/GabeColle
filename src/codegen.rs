//
// TR-SECD code generator
//

use crate::lambda::*;
use crate::secd::Dumps;
use crate::secd::CodeElem::*;

pub fn generate(t: Term) -> Dumps {
    Dumps::State(vec![], vec![], vec![Ret, LTerm(t)], Box::new(Dumps::Halt))
}

