use std::fmt;
use std::rc;

pub trait Pretty {
    fn pp(&self, f: &mut fmt::Formatter<'_>, prec: usize) -> fmt::Result;
}

impl<T: Pretty> Pretty for rc::Rc<T> {
    fn pp(&self, f: &mut fmt::Formatter<'_>, prec: usize) -> fmt::Result {
        self.as_ref().pp(f, prec)
    }
}

impl Pretty for str {
    fn pp(&self, f: &mut fmt::Formatter<'_>, _prec: usize) -> fmt::Result {
        write!(f, "{}", self)
    }
}

pub fn parens<F>(f: &mut fmt::Formatter<'_>, cond: bool, body: F) -> fmt::Result
where
    F: FnOnce(&mut fmt::Formatter<'_>) -> fmt::Result,
{
    if cond {
        write!(f, "(")?;
    }
    let res = body(f);
    if cond {
        write!(f, ")")?;
    }
    res
}

pub fn commas<T>(f: &mut fmt::Formatter<'_>, xs: &[T]) -> fmt::Result
where
    T: Pretty + Sized,
{
    if xs.is_empty() {
        return Ok(());
    }

    let last = xs.len() - 1;
    for x in &xs[..last] {
        x.pp(f, 0)?;
        write!(f, ", ")?;
    }

    xs[last].pp(f, 0)
}
