type Name = String;

#[derive(Debug)]
pub enum Lit {
    Number(f64),
    String(String),
}

#[derive(Debug)]
pub enum Atom {
    Literal(Lit),
    Id(Name),
    Lambda(i32, Vec<Expr>),
}

#[derive(Debug)]
pub enum Expr {
    Atom(Atom),
    DBI(i32),
    Unary(char, Box<Expr>),
    Binary(char, Box<Expr>, Box<Expr>),
    Apply(Box<Expr>, Vec<Expr>),
}

#[derive(Debug)]
pub enum Decl {
    Let(Name, Expr),
}

#[derive(Debug)]
pub enum ProgramItem {
    Expr(Expr),
    Decl(Decl),
}

type Program = Vec<ProgramItem>;
