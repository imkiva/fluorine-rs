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
    _InternalError,

    AtomExpr(Atom),
    DBI(i32),
    UnaryExpr(String, Box<Expr>),
    BinaryExpr(String, Box<Expr>, Box<Expr>),
    ApplyExpr(Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
pub enum Decl {
    LetDecl(Name, Expr),
}

#[derive(Debug)]
pub enum ProgramItem {
    ExprItem(Expr),
    DeclItem(Decl),
    EOFItem,
}

pub type Program = Vec<ProgramItem>;
