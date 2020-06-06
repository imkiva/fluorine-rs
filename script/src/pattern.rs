use crate::{
    eval::{
        Value,
        Value::{BoolValue, NumberValue, StringValue},
    },
    tree::{
        Expr,
        Lit::{LitBool, LitNumber, LitString},
        MatchCase, Pattern,
    },
};

pub(crate) trait Matcher {
    type Input;
    type Records;
    type Selected;

    fn try_match(self, input: &Self::Input) -> Option<(Self::Records, Self::Selected)>;
}

impl Matcher for MatchCase {
    type Input = Value;
    // XXX: No record currently
    type Records = ();
    type Selected = Expr;

    fn try_match(self, input: &Self::Input) -> Option<(Self::Records, Self::Selected)> {
        match (self.0, input) {
            (Pattern::PatternWildcard, _) => Some(((), self.1)),
            (Pattern::PatternLit(LitBool(lhs)), BoolValue(rhs)) if lhs == *rhs => {
                Some(((), self.1))
            }
            (Pattern::PatternLit(LitNumber(lhs)), NumberValue(rhs)) if lhs == *rhs => {
                Some(((), self.1))
            }
            (Pattern::PatternLit(LitString(lhs)), StringValue(rhs)) if lhs == *rhs => {
                Some(((), self.1))
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
