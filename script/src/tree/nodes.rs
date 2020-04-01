pub type Name = String;

#[derive(Debug)]
pub enum Lit {
    LitNumber(f64),
    LitString(String),
    LitBool(bool),
}

#[derive(Debug)]
pub enum Atom {
    AtomLit(Lit),
    AtomId(Name),
    AtomLambda(i32, Vec<Expr>),
    AtomRawLambda(Vec<Name>, Vec<Expr>),
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
