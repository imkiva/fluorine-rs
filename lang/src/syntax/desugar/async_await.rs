use crate::syntax::tree::{
    Atom, Decl, EnumVariant, Expr, Ident, MatchCase, Param, ParseType, PatEnumVariant, Pattern,
    Program, ProgramItem,
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

    let start_state_name = Expr::AtomExpr(Atom::AtomId(
        format!("AsyncStateMachine{}State0", id)
    ));
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

    for i in 1..states.len() {
        let mut params = fields.clone();
        params.push(Ident::from("result"));
        cases.push(MatchCase(
            Pattern::PatVariant(PatEnumVariant {
                name: format!("AsyncStateMachine{}State{}", id, i),
                fields: params,
            }),
            // TODO: support expr_list on match case
            states[i][0].clone(),
        ));
    }

    // case end state
    cases.push(MatchCase(
        Pattern::PatVariant(PatEnumVariant {
            name: format!("AsyncStateMachine{}State{}", id, states.len() + 2),
            fields: vec![],
        }),
        // TODO: support expr_list on match case
        states[0][0].clone(),
    ));

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

    // end state
    variants.push(EnumVariant {
        name: format!("AsyncStateMachine{}State{}", id, states + 1),
        field_types: vec![],
    });

    variants
}

fn split_states(id: usize, body: Vec<Expr>, params: &Vec<Param>) -> Vec<Vec<Expr>> {
    let mut states = Vec::with_capacity(body.len());
    states.push(Vec::new());

    for expr in body {
        match expr {
            Expr::AwaitExpr(e) => {
                let next_state = states.len() - 1;
                states
                    .last_mut()
                    .unwrap()
                    .push(desugar_await(id, next_state, params, *e));
                states.push(Vec::new());
            }
            _ => states.last_mut().unwrap().push(expr),
        }
    }

    states.into_iter()
        .filter(|exprs| !exprs.is_empty())
        .collect()
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
    //     Pending  => self,
    // }
    Expr::MatchExpr(
        Box::new(poll),
        vec![
            MatchCase(
                Pattern::PatVariant(PatEnumVariant {
                    name: Ident::from("Ready"),
                    fields: vec![Ident::from("result")],
                }),
                create_next_state,
            ),
            MatchCase(
                Pattern::PatVariant(PatEnumVariant {
                    name: Ident::from("Pending"),
                    fields: vec![],
                }),
                Expr::AtomExpr(Atom::AtomId(Ident::from("self"))),
            ),
        ],
    )
}
