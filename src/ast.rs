#[derive(Debug)]
pub enum Decl {
    Data(DataDecl),
}

#[derive(Debug)]
pub struct DataDecl {
    pub name: String,
}
