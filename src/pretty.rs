use std::fmt;
use std::rc;

pub enum PP {
    VCat { above: Box<PP>, below: Box<PP> },
    HCat { left: Box<PP>, right: Box<PP> },
    Str { value: String },
}

impl PP {
    pub fn vcat(above: PP, below: PP) -> Self {
        PP::VCat {
            above: Box::new(above),
            below: Box::new(below),
        }
    }

    pub fn hcat(left: PP, right: PP) -> Self {
        PP::HCat {
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    pub fn from(value: &str) -> Self {
        PP::Str{ value: String::from(value) }
    }

    pub fn cols(&self) -> usize {
        match self {
            PP::VCat { above, below } => above.cols().max(below.cols()),
            PP::HCat { left, right } => left.cols() + right.cols(),
            PP::Str { value } => value.len(),
        }
    }

    fn to_lines(&self, indent: usize, out: &mut Vec<String>) {
        match self {
            PP::VCat { above, below } => {
                above.to_lines(indent, out);
                below.to_lines(indent, out);
            }

            PP::HCat { left, right } => {
                let start = out.len();
                left.to_lines(indent, out);
                let left_len = out.len() - start;

                let mut rbuf = Vec::new();
                let rindent = indent + left.cols();
                right.to_lines(rindent, &mut rbuf);

                let min_len = left_len.min(rbuf.len());
                for i in 0..min_len {
                    out[start + i].push_str(&rbuf[i]);
                }

                if left_len < rbuf.len() {
                    let spaces: String = std::iter::repeat(' ').take(rindent).collect();
                    for i in left_len..rbuf.len() {
                        let mut buf = spaces.clone();
                        buf.push_str(&rbuf[i]);
                        out.push(buf);
                    }
                }
            }

            PP::Str { value } => {
                let mut buf: String = std::iter::repeat(' ').take(indent).collect();
                buf.push_str(&value);
                out.push(buf);
            }
        }
    }
}

impl fmt::Display for PP {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut buf = Vec::new();
        self.to_lines(0, &mut buf);
        for line in buf {
            writeln!(f, "{}", line)?;
        }
        Ok(())
    }
}

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
