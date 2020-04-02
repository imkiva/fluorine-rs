pub type Name = String;

type Argc = i32;
type ApplyStartDBI = i32;

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Lit {
    LitNumber(f64),
    LitString(String),
    LitBool(bool),
}

#[derive(Debug, Clone)]
pub enum Atom {
    AtomLit(Lit),
    AtomId(Name),
    AtomLambda(Argc, ApplyStartDBI, Vec<Expr>),
    AtomRawLambda(Vec<Name>, Vec<Expr>),
}

#[derive(Debug, Clone)]
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
