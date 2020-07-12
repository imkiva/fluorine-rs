use crate::{ffi::*, runtime::Context};

pub struct Builtins {}

impl Builtins {
    pub fn init(ctx: &mut Context) {
        fluorine_module_entrance_for_Prelude()
            .into_iter()
            .for_each(|(k, v)| {
                ctx.ffi(k, v.argc, v.closure).unwrap();
            });

        fluorine_module_entrance_for_Math()
            .into_iter()
            .for_each(|(k, v)| {
                ctx.ffi(k, v.argc, v.closure).unwrap();
            });
    }
}

#[fluorine(Prelude)]
mod prelude_ffi {
    use crate::{ffi::*, runtime::Value};

    #[fluorine]
    fn panic(reason: String) -> FFIError {
        FFIError::Panic(reason)
    }

    #[fluorine]
    fn id(v: Value) -> Value {
        v
    }

    #[fluorine]
    fn println(v: Value) {
        println!("{}", v);
    }
}

#[fluorine(Math)]
mod math_ffi {
    use crate::{
        ffi::*,
        runtime::{builtins::math_ffi::derive_impl::Derive1, FromValue, IntoValue, Value},
        syntax::{pe::PartialEval, tree::Expr},
    };

    struct LambdaValue {
        argc: usize,
        dbi: usize,
        body: Vec<Expr>,
    }

    impl FromValue for LambdaValue {
        fn from_value(value: Value) -> Option<Self> {
            match value {
                Value::LambdaValue(argc, dbi, body) => Some(LambdaValue { argc, dbi, body }),
                _ => None,
            }
        }
    }

    impl IntoValue for LambdaValue {
        fn into_value(self) -> Value {
            Value::LambdaValue(self.argc, self.dbi, self.body)
        }
    }

    mod derive_impl {
        use crate::syntax::tree::{Atom, Expr, Lit, MatchCase, Pattern};

        pub trait Derive1 {
            fn derive1(self) -> Self;
        }

        impl<T> Derive1 for Vec<T>
        where
            T: Derive1,
        {
            fn derive1(self) -> Self {
                self.into_iter().map(|t| t.derive1()).collect()
            }
        }

        impl<T> Derive1 for Box<T>
        where
            T: Derive1,
        {
            fn derive1(self) -> Self {
                Box::new((*self).derive1())
            }
        }

        impl Derive1 for Expr {
            fn derive1(self) -> Self {
                match self {
                    Expr::Unit => Expr::Unit,
                    Expr::AtomExpr(atom) => Expr::AtomExpr(atom.derive1()),
                    Expr::DBI(_) => Expr::AtomExpr(Atom::AtomLit(Lit::LitNumber(1.0))),
                    Expr::UnaryExpr(op, expr) => Expr::UnaryExpr(op, expr.derive1()),
                    Expr::ApplyExpr(f, a) => Expr::ApplyExpr(f.derive1(), a.derive1()),
                    Expr::MatchExpr(matchee, cases) => {
                        Expr::MatchExpr(matchee.derive1(), cases.derive1())
                    }
                    Expr::MemberExpr(lhs, id) => Expr::MemberExpr(lhs.derive1(), id),

                    Expr::BinaryExpr(op, lhs, rhs) => match (op.as_str(), *lhs, *rhs) {
                        ("*", lhs, rhs) => Expr::BinaryExpr(
                            "+".to_string(),
                            Box::new(Expr::BinaryExpr(
                                "*".to_string(),
                                Box::new(lhs.clone().derive1()),
                                Box::new(rhs.clone()),
                            )),
                            Box::new(Expr::BinaryExpr(
                                "*".to_string(),
                                Box::new(lhs),
                                Box::new(rhs.derive1()),
                            )),
                        ),

                        ("/", lhs, rhs) => Expr::BinaryExpr(
                            "/".to_string(),
                            Box::new(Expr::BinaryExpr(
                                "-".to_string(),
                                Box::new(Expr::BinaryExpr(
                                    "*".to_string(),
                                    Box::new(lhs.clone().derive1()),
                                    Box::new(rhs.clone()),
                                )),
                                Box::new(Expr::BinaryExpr(
                                    "*".to_string(),
                                    Box::new(lhs),
                                    Box::new(rhs.clone().derive1()),
                                )),
                            )),
                            Box::new(Expr::BinaryExpr(
                                "^".to_string(),
                                Box::new(rhs),
                                Box::new(Expr::AtomExpr(Atom::AtomLit(Lit::LitNumber(2.0)))),
                            )),
                        ),

                        (_, lhs, rhs) => Expr::BinaryExpr(op, Box::new(lhs), Box::new(rhs)),
                    },
                }
            }
        }

        impl Derive1 for Atom {
            fn derive1(self) -> Self {
                match self {
                    Atom::AtomLit(lit) => Atom::AtomLit(lit.derive1()),
                    atom => atom,
                }
            }
        }

        impl Derive1 for Lit {
            fn derive1(self) -> Self {
                match self {
                    Lit::LitNumber(_) => Lit::LitNumber(0.0),
                    Lit::LitString(s) => Lit::LitString(s),
                    Lit::LitBool(b) => Lit::LitBool(b),
                }
            }
        }

        impl Derive1 for MatchCase {
            fn derive1(self) -> Self {
                MatchCase(self.0.derive1(), self.1.derive1())
            }
        }

        impl Derive1 for Pattern {
            fn derive1(self) -> Self {
                match self {
                    Pattern::PatLit(lit) => Pattern::PatLit(lit.derive1()),
                    pat => pat,
                }
            }
        }
    }

    #[fluorine]
    fn derive(f: LambdaValue) -> Result<LambdaValue, FFIError> {
        if f.argc - f.dbi != 1 {
            return Err(FFIError::CustomError(
                "derive() can only handle 1-argument functions".to_string(),
            ));
        }

        Ok(LambdaValue {
            argc: f.argc,
            dbi: f.dbi,
            body: f.body.derive1().partial_eval(),
        })
    }
}
