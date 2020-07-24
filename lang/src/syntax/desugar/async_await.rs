use crate::{
    runtime::subst::Subst,
    syntax::tree::{
        Atom, Decl, EnumVariant, Expr, Ident, MatchCase, Param, ParseType, PatEnumVariant, Pattern,
        Program, ProgramItem,
    },
};
use lazy_static::lazy_static;
use spin::Mutex;

lazy_static! {
    static ref ASYNC_LAMBDA_COUNT: Mutex<usize> = Mutex::new(0);
}

pub fn desugar_async_lambda(params: Vec<Param>, body: Vec<Expr>, extra: &mut Program) -> Expr {
    let id = new_state_machine_id();

    let states = split_states(id, body, &params);
    let variants = generate_state_variants(id, states.len(), &params);

    let state_machine_enum = Decl::EnumDecl(
        Vec::with_capacity(0),
        format!("AsyncStateMachine{}", id),
        variants,
    );

    // declare state machine
    extra.push(ProgramItem::DeclItem(state_machine_enum));
    // implement Future trait for state machine
    let pool_body = build_poll_lambda(id, states, &params);
    extra.push(ProgramItem::DeclItem(Decl::ImplDecl(
        vec![],
        Ident::from("Future"),
        format!("AsyncStateMachine{}", id),
        vec![Decl::LetDecl(Ident::from("poll"), pool_body)],
    )));

    let start_state_name = Expr::AtomExpr(Atom::AtomId(format!("AsyncStateMachine{}State0", id)));
    let create_start_state = (0..params.len())
        .into_iter()
        .map(|dbi| Expr::DBI(dbi))
        .map(|expr| Box::new(expr))
        .fold(start_state_name, |lhs, arg| {
            Expr::ApplyExpr(Box::new(lhs), arg)
        });

    Expr::AtomExpr(Atom::AtomLambda(params, 0, vec![create_start_state]))
}

fn build_poll_lambda(id: usize, states: Vec<Vec<Expr>>, params: &Vec<Param>) -> Expr {
    let fields = params.clone().into_iter().map(|p| p.id).collect::<Vec<_>>();
    let mut cases = Vec::new();

    // case start state
    cases.push(MatchCase(
        Pattern::PatVariant(PatEnumVariant {
            name: format!("AsyncStateMachine{}State0", id),
            fields: fields.clone(),
        }),
        // TODO: support expr_list on match case
        states[0][0].clone(),
    ));

    for i in 1..=states.len() {
        let mut params = fields.clone();
        params.push(Ident::from("result"));
        cases.push(MatchCase(
            Pattern::PatVariant(PatEnumVariant {
                name: format!("AsyncStateMachine{}State{}", id, i),
                fields: params,
            }),
            // TODO: support expr_list on match case
            match states.get(i).map(|it| it.first()) {
                // is intermediate state
                Some(Some(e)) => e.clone(),
                // is end state
                _ => Expr::ApplyExpr(
                    Box::new(Expr::AtomExpr(Atom::AtomId(Ident::from("Ready")))),
                    Box::new(Expr::AtomExpr(Atom::AtomId(Ident::from("result"))))
                ),
            },
        ));
    }

    // pool: (self, unit: Unit) -> Poll;
    let poll_body = Expr::MatchExpr(Box::new(Expr::DBI(0)), cases);
    Expr::AtomExpr(Atom::AtomLambda(
        vec![
            Param {
                id: Ident::from("self"),
                ty: Some(ParseType::SelfType),
            },
            Param {
                id: Ident::from("unit"),
                ty: Some(ParseType::OtherType(Ident::from("Unit"))),
            },
        ],
        0,
        vec![poll_body],
    ))
}

fn create_state_enum_variant(name: String, args: Vec<Ident>) -> Expr {
    let enum_name = Expr::AtomExpr(Atom::AtomId(name));

    args.into_iter()
        .map(|id| Expr::AtomExpr(Atom::AtomId(id)))
        .map(|expr| Box::new(expr))
        .fold(enum_name, |lhs, arg| Expr::ApplyExpr(Box::new(lhs), arg))
}

fn new_state_machine_id() -> usize {
    let mut async_lambda_count = ASYNC_LAMBDA_COUNT.lock();
    let unique_state_machine_id = *async_lambda_count;
    *async_lambda_count += 1;
    unique_state_machine_id
}

fn generate_state_variants(id: usize, states: usize, params: &Vec<Param>) -> Vec<EnumVariant> {
    let mut variants = Vec::with_capacity(2 + states);

    let param_types: Vec<Ident> = params
        .iter()
        .map(|p| match &p.ty {
            Some(ParseType::SelfType) => unreachable!("enum variant should not be Self"),
            Some(ParseType::OtherType(id)) => id.clone(),
            _ => Ident::from("Any"),
        })
        .collect();

    // start state
    variants.push(EnumVariant {
        name: format!("AsyncStateMachine{}State0", id),
        field_types: param_types.clone(),
    });

    // we need one start state and one end state
    for i in 0..states {
        let mut param_types = param_types.clone();
        param_types.push(Ident::from("Any"));
        variants.push(EnumVariant {
            name: format!("AsyncStateMachine{}State{}", id, i + 1),
            field_types: param_types,
        });
    }

    variants
}

fn split_states(id: usize, body: Vec<Expr>, params: &Vec<Param>) -> Vec<Vec<Expr>> {
    let mut states = Vec::with_capacity(body.len());
    states.push(Vec::new());

    for expr in body {
        match expr {
            Expr::AwaitExpr(e) => {
                let expr = subst_params(*e, params);
                let next_state = states.len();
                states
                    .last_mut()
                    .unwrap()
                    .push(desugar_await(id, next_state, params, expr));
                states.push(Vec::new());
            }
            _ => {
                let expr = subst_params(expr, params);
                states.last_mut().unwrap().push(expr);
            }
        }
    }

    states.into_iter()
        .filter(|e| !e.is_empty())
        .collect()
}

fn subst_params(e: Expr, params: &Vec<Param>) -> Expr {
    (0..params.len()).into_iter().fold(e, |expr, dbi| {
        expr.subst(dbi, &Expr::AtomExpr(Atom::AtomId(params[dbi].id.clone())))
    })
}

fn desugar_await(id: usize, next_state: usize, params: &Vec<Param>, e: Expr) -> Expr {
    // pool: (self, unit: Unit) -> Poll;
    let poll = Expr::ApplyExpr(
        Box::new(Expr::MemberExpr(Box::new(e), Ident::from("poll"))),
        Box::new(Expr::Unit),
    );

    let mut params = params.clone().into_iter().map(|p| p.id).collect::<Vec<_>>();
    params.push(Ident::from("result"));

    let create_next_state = create_state_enum_variant(
        format!("AsyncStateMachine{}State{}", id, next_state),
        params,
    );

    // match e.poll(()) {
    //     Ready(e) => NextState(...),
    //     Pending(_)  => Pending(self),
    // }
    Expr::MatchExpr(
        Box::new(poll),
        vec![
            MatchCase(
                Pattern::PatVariant(PatEnumVariant {
                    name: Ident::from("Ready"),
                    fields: vec![Ident::from("result")],
                }),
                Expr::ApplyExpr(
                    Box::new(Expr::MemberExpr(
                        Box::new(create_next_state),
                        Ident::from("poll"),
                    )),
                    Box::new(Expr::Unit),
                ),
            ),
            MatchCase(
                Pattern::PatVariant(PatEnumVariant {
                    name: Ident::from("Pending"),
                    fields: vec![Ident::from("last")],
                }),
                Expr::ApplyExpr(
                    Box::new(Expr::AtomExpr(Atom::AtomId(Ident::from("Pending")))),
                    Box::new(Expr::DBI(0)),
                ),
            ),
        ],
    )
}
