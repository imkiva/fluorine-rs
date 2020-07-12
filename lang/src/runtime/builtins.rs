use crate::{ffi::*, runtime::Context};

pub struct Builtins {}

impl Builtins {
    pub fn init(ctx: &mut Context) {
        fluorine_module_entrance_for_Prelude()
            .into_iter()
            .for_each(|(k, v)| {
                ctx.ffi(k, v).unwrap();
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
