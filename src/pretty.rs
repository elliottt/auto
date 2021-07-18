pub use pretty::RcDoc;
use std::rc;

pub trait Pretty {
    fn pp(&self, prec: usize) -> RcDoc;
}

impl<T: Pretty> Pretty for rc::Rc<T> {
    fn pp(&self, prec: usize) -> RcDoc {
        self.as_ref().pp(prec)
    }
}

impl Pretty for str {
    fn pp(&self, _prec: usize) -> RcDoc {
        RcDoc::text(self)
    }
}

pub fn parens(cond: bool, body: RcDoc) -> RcDoc {
    RcDoc::concat([
        if cond { RcDoc::text("(") } else { RcDoc::nil() },
        body,
        if cond { RcDoc::text(")") } else { RcDoc::nil() },
    ])
}
