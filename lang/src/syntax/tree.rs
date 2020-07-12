pub type Ident = String;

pub type DBI = usize;
pub type Argc = DBI;
pub type ApplyStartDBI = DBI;

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Param {
    pub id: Ident,
    pub ty: Option<Ident>,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Lit {
    LitNumber(f64),
    LitString(String),
    LitBool(bool),
}

#[derive(Debug, Clone)]
pub enum Atom {
    AtomLit(Lit),
    AtomId(Ident),
    AtomLambda(Vec<Param>, ApplyStartDBI, Vec<Expr>),
    AtomRawLambda(Vec<Param>, Vec<Expr>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Unit,
    AtomExpr(Atom),
    DBI(usize),
    UnaryExpr(String, Box<Expr>),
    BinaryExpr(String, Box<Expr>, Box<Expr>),
    ApplyExpr(Box<Expr>, Box<Expr>),
    MemberExpr(Box<Expr>, Ident),
    MatchExpr(Box<Expr>, Vec<MatchCase>),
}

#[derive(Debug, Clone)]
pub struct MatchCase(pub Pattern, pub Expr);

#[derive(Debug, Clone)]
pub enum Pattern {
    PatLit(Lit),
    PatVariant(PatEnumVariant),
    PatWildcard,
}

#[derive(Debug)]
pub enum Decl {
    LetDecl(Ident, Expr),
    EnumDecl(Ident, Vec<EnumVariant>),
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone)]
pub struct EnumVariant {
    pub name: String,
    pub fields: usize,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone)]
pub struct PatEnumVariant {
    pub name: String,
    pub fields: Vec<String>,
}

#[derive(Debug)]
pub enum ProgramItem {
    ExprItem(Expr),
    DeclItem(Decl),
}

pub type Program = Vec<ProgramItem>;
