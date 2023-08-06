#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct StringRef {
    offset: u32,
    length: u16,
}

impl StringRef {
    pub fn new(offset: usize, length: usize) -> Self {
        StringRef {
            offset: offset as u32,
            length: length as u16,
        }
    }
}

impl Into<std::ops::Range<usize>> for StringRef {
    fn into(self) -> std::ops::Range<usize> {
        let start = self.offset as usize;
        let end = start + (self.length as usize);
        start..end
    }
}

pub struct Strings {
    data: String,
}

impl Strings {
    pub fn new() -> Self {
        Strings {
            data: String::new(),
        }
    }

    /// Lookup a [StringRef] in the table.
    pub fn index(&self, sref: StringRef) -> &str {
        let range: std::ops::Range<usize> = sref.into();
        &self.data[range]
    }

    /// Intern a string.
    pub fn intern<S: AsRef<str>>(&mut self, string: S) -> StringRef {
        let string = string.as_ref();
        if let Some(offset) = self.data.find(string) {
            StringRef::new(offset, string.len())
        } else {
            let offset = self.data.len();
            self.data.push_str(string);
            StringRef::new(offset, string.len())
        }
    }
}
