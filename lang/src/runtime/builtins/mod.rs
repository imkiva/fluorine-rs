use crate::{
    ffi::*,
    runtime::{Context, Type, Value},
    syntax::optimize::OptimizeLevel,
    Compiler,
};
use std::collections::VecDeque;

macro_rules! map(
    { $($key:expr => $value:expr),+ } => {
        {
            let mut m = ::std::collections::HashMap::new();
            $(
                m.insert($key.to_string(), $value);
            )+
            m
        }
     };
);

macro_rules! impls {
    ($ctx: expr, $tr:expr, $ty: expr, $fns: expr) => {
        $ctx.impl_trait($tr.to_string(), $ty, $fns)
            .expect("Prelude Source Code Error");
    };
}

pub struct Builtins {}

impl Builtins {
    pub fn init(ctx: &mut Context) {
        let prelude_decl = include_str!("prelude.fss");
        let ast = Compiler::compile(OptimizeLevel::Normal, prelude_decl)
            .expect("Prelude Source Code Error");
        ctx.source(ast).expect("Prelude Source Code Error");

        let binary = |f: FFIFn| {
            let binary_param = vec![
                Param {
                    id: "self".to_string(),
                    ty: Some(ParseType::SelfType),
                },
                Param {
                    id: "rhs".to_string(),
                    ty: Some(ParseType::SelfType),
                },
            ];
            Value::ForeignLambda(
                FFIClosure {
                    param: binary_param,
                    closure: f,
                },
                VecDeque::with_capacity(2),
            )
        };

        let unary = |f: FFIFn| {
            let unary_param = vec![
                Param {
                    id: "self".to_string(),
                    ty: Some(ParseType::SelfType),
                },
                Param {
                    id: "rhs".to_string(),
                    ty: Some(ParseType::OtherType("Unit".to_string())),
                },
            ];
            Value::ForeignLambda(
                FFIClosure {
                    param: unary_param,
                    closure: f,
                },
                VecDeque::with_capacity(2),
            )
        };

        impls!(
            ctx,
            "Add",
            Type::NumberType,
            map! {
                "add" => binary(prelude_traits::add_number)
            }
        );

        impls!(
            ctx,
            "Sub",
            Type::NumberType,
            map! {
                "sub" => binary(prelude_traits::sub_number)
            }
        );

        impls!(
            ctx,
            "Mul",
            Type::NumberType,
            map! {
                "mul" => binary(prelude_traits::mul_number)
            }
        );

        impls!(
            ctx,
            "Div",
            Type::NumberType,
            map! {
                "div" => binary(prelude_traits::div_number)
            }
        );

        impls!(
            ctx,
            "Mod",
            Type::NumberType,
            map! {
                "mod" => binary(prelude_traits::mod_number)
            }
        );

        impls!(
            ctx,
            "Pow",
            Type::NumberType,
            map! {
                "pow" => binary(prelude_traits::pow_number)
            }
        );

        impls!(
            ctx,
            "Not",
            Type::BoolType,
            map! {
                "not" => unary(prelude_traits::not_bool)
            }
        );

        impls!(
            ctx,
            "Add",
            Type::StringType,
            map! {
                "add" => binary(prelude_traits::add_string)
            }
        );

        fluorine_module_entrance_for_Prelude()
            .into_iter()
            .for_each(|(k, v)| {
                ctx.ffi(k, v).unwrap();
            });
    }
}

mod prelude_traits {
    use crate::ffi::*;

    type Number = f64;

    #[fluorine]
    fn add_string(lhs: String, rhs: String) -> String {
        format!("{}{}", lhs.as_str(), rhs.as_str())
    }

    #[fluorine]
    fn add_number(lhs: Number, rhs: Number) -> Number {
        return lhs + rhs;
    }

    #[fluorine]
    fn sub_number(lhs: Number, rhs: Number) -> Number {
        return lhs - rhs;
    }

    #[fluorine]
    fn mul_number(lhs: Number, rhs: Number) -> Number {
        return lhs * rhs;
    }

    #[fluorine]
    fn div_number(lhs: Number, rhs: Number) -> Number {
        return lhs / rhs;
    }

    #[fluorine]
    fn pow_number(lhs: Number, rhs: Number) -> Number {
        return lhs.powf(rhs);
    }

    #[fluorine]
    fn mod_number(lhs: Number, rhs: Number) -> Number {
        return lhs % rhs;
    }

    #[fluorine]
    fn not_bool(lhs: bool, _: ()) -> bool {
        !lhs
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
        match v {
            Value::UnitValue => println!("()"),
            Value::NumberValue(n) => println!("{}", n),
            Value::BoolValue(b) => println!("{}", b),
            Value::StringValue(s) => println!("{}", s),
            v => println!("{}", v),
        }
    }
}
