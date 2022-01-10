use super::Inferer;
use super::Type;
use crate::define;

impl Inferer {
    pub fn init(&mut self) {
        self.register_builtin_function_type();
    }
    pub fn register_builtin_function_type(&mut self) {
        let int_type = self.push_new_type(Type::Int(None));
        let fn_int_int = self.push_new_type(Type::Fn(int_type, int_type));
        let fn_int_int_int = self.push_new_type(Type::Fn(int_type, fn_int_int));

        let real_type = self.push_new_type(Type::Real(None));
        let fn_real_real = self.push_new_type(Type::Fn(real_type, real_type));
        let fn_real_real_real = self.push_new_type(Type::Fn(real_type, fn_real_real));

        let fn_int_real_real = self.push_new_type(Type::Fn(int_type, fn_real_real));

        let fn_int_real = self.push_new_type(Type::Fn(int_type, real_type));
        let fn_real_int_real = self.push_new_type(Type::Fn(real_type, fn_int_real));

        let string_type = self.push_new_type(Type::String(None));
        let fn_string_string = self.push_new_type(Type::Fn(string_type, string_type));
        let fn_string_string_string = self.push_new_type(Type::Fn(string_type, fn_string_string));

        let add_fn_type = self.push_new_type(Type::Union(vec![
            fn_int_int_int,
            fn_real_real_real,
            fn_int_real_real,
            fn_real_int_real,
            fn_string_string_string,
        ].into_iter().collect()));
        self.context.env[0].insert(define::ADD.to_owned(), add_fn_type);

        self.context.env[0].insert("add".to_owned(), add_fn_type);
        let any_type = self.push_new_type(Type::Any);
        let fn_any_any = self.push_new_type(Type::Fn(any_type, any_type));
        self.context.env[0].insert(define::LET.to_owned(), fn_any_any);
    }
}
