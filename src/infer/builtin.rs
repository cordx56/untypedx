use super::Inferer;
use super::Type;
use crate::define;

impl Inferer {
    pub fn init(&mut self) {
        self.register_builtin_function_type();
    }
    pub fn register_builtin_function_type(&mut self) {
        self.context.env[0].insert(
            define::ADD.to_owned(),
            Type::Fn(
                Box::new(Type::Int(None)),
                Box::new(Type::Fn(
                    Box::new(Type::Int(None)),
                    Box::new(Type::Int(None)),
                )),
            ),
        );
        self.context.env[0].insert(
            "add".to_owned(),
            Type::Fn(
                Box::new(Type::Int(None)),
                Box::new(Type::Fn(
                    Box::new(Type::Int(None)),
                    Box::new(Type::Int(None)),
                )),
            ),
        );
        self.context.env[0].insert(
            define::LET.to_owned(),
            Type::Fn(
                Box::new(Type::Any),
                Box::new(Type::Any),
            ),
        );
    }
}
