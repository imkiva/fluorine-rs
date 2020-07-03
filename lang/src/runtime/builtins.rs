use crate::{ffi::*, runtime::Context};
use std::collections::HashMap;

pub struct Builtins {}

impl Builtins {
    pub fn init(ctx: &mut Context) {
        let map: HashMap<String, FFIClosure> = fluorine_module_entrance_for_Builtin();
        map.into_iter().for_each(|(k, v)| {
            ctx.ffi(k, v.argc, v.closure).unwrap();
        });
    }
}

#[fluorine(Builtin)]
mod builtin_ffi {
    use crate::{ffi::*, runtime::Value};

    #[fluorine]
    fn panic(reason: String) -> FFIError {
        FFIError::Panic(reason)
    }

    #[fluorine]
    fn id(v: Value) -> Value {
        v
    }
}
