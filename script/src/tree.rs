pub type Name = String;

pub type DBI = i32;
pub type Argc = DBI;
pub type ApplyStartDBI = DBI;

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
    _BottomExpr,

    AtomExpr(Atom),
    DBI(i32),
    UnaryExpr(String, Box<Expr>),
    BinaryExpr(String, Box<Expr>, Box<Expr>),
    ApplyExpr(Box<Expr>, Box<Expr>),
    MatchExpr(Box<Expr>, Vec<MatchCase>),
}

#[derive(Debug, Clone)]
pub struct MatchCase(pub Pattern, pub Expr);

#[derive(Debug, Clone)]
pub enum Pattern {
    PatternLit(Lit),
    PatternWildcard,
}

#[derive(Debug)]
pub enum Decl {
    LetDecl(Name, Expr),
}

#[derive(Debug)]
pub enum ProgramItem {
    ExprItem(Expr),
    DeclItem(Decl),
}

pub type Program = Vec<ProgramItem>;
