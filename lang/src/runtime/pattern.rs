use crate::{
    runtime::{
        Value,
        Value::{BoolValue, EnumValue, NumberValue, StringValue},
    },
    syntax::tree::{
        Expr,
        Lit::{LitBool, LitNumber, LitString},
        MatchCase,
        Pattern::{PatLit, PatVariant, PatWildcard},
    },
};
use std::collections::HashMap;

pub(crate) trait Matcher {
    type Input;
    type Records;
    type Selected;

    fn try_match(self, input: &Self::Input) -> Option<(Self::Records, Self::Selected)>;
}

impl Matcher for MatchCase {
    type Input = Value;
    type Records = HashMap<String, Value>;
    type Selected = Expr;

    fn try_match(self, input: &Self::Input) -> Option<(Self::Records, Self::Selected)> {
        match (self.0, input) {
            (PatWildcard, _) => Some((Default::default(), self.1)),
            (PatLit(LitBool(lhs)), BoolValue(rhs)) if lhs == *rhs => {
                Some((Default::default(), self.1))
            }
            (PatLit(LitNumber(lhs)), NumberValue(rhs)) if lhs == *rhs => {
                Some((Default::default(), self.1))
            }
            (PatLit(LitString(lhs)), StringValue(rhs)) if lhs == *rhs => {
                Some((Default::default(), self.1))
            }
            (PatVariant(lhs), EnumValue(ty, rhs, fields))
                if ty.has_pat_variant(&lhs)
                    && lhs.name == rhs.name
                    && lhs.fields.len() == fields.len() =>
            {
                let records = lhs.fields.into_iter().zip(fields.clone()).collect();
                Some((records, self.1))
            }
            _ => None,
        }
    }
}

impl<T: Matcher> Matcher for Vec<T> {
    type Input = T::Input;
    type Records = T::Records;
    type Selected = T::Selected;

    fn try_match(self, input: &Self::Input) -> Option<(Self::Records, Self::Selected)> {
        self.into_iter().fold(None, |last, matcher| match last {
            None => matcher.try_match(input),
            _ => last,
        })
    }
}
