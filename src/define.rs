pub static VERSION: &str = "0.1.0";
// Special
pub static EXPRESSION_OPEN: &str = "(";
pub static EXPRESSION_CLOSE: &str = ")";
pub static BLOCK_OPEN: &str = "{";
pub static BLOCK_CLOSE: &str = "}";
pub static INDEX_OPEN: &str = "[";
pub static INDEX_CLOSE: &str = "]";
pub static PARSER_NOT_IDENTIFIER: &str = " \t\r\n(){}[]<>:,|";
pub static STRING_QUOTE: &str = "\"";
pub static STATEMENT_END: &str = "\n";
pub static FN: &str = "fn";
pub static FN_ARROW: &str = "=>";
pub static FN_OR: &str = "|";
// Tuple
pub static TUPLE_OPEN: &str = "(";
pub static TUPLE_CLOSE: &str = ")";
pub static TUPLE_DELIMITER: &str = ",";

// Type name
pub static STRING: &str = "string";
pub static INT: &str = "int";
pub static FLOAT: &str = "float";
pub static BOOL: &str = "bool";
pub static VECTOR: &str = "vector";
pub static MAP: &str = "map";
pub static FUNCTION: &str = "function";
pub static VOID: &str = "void";

pub static FUNCTION_TYPE_ARROW: &str = "->";
pub static TYPE_ANNOTATION: &str = ":";

// Pattern match
pub static PAT_WILDCARD: &str = "_";

// Variables
pub static TRUE: &str = "true";
pub static FALSE: &str = "false";

// Functions
pub static IMPORT: &str = "import";
pub static UNTYPED: &str = "untyped";
pub static LET: &str = "let";
pub static ASSIGN: &str = "=";
pub static ASSIGN_DEFER: &str = "defer=";
pub static RETURN: &str = "return";
pub static BREAK: &str = "break";
pub static CONTINUE: &str = "continue";
pub static PRINT: &str = "print";
pub static PRINTLN: &str = "println";
pub static VALUE: &str = "value";
pub static MAKE_VECTOR: &str = "make_vector";
pub static MAKE_MAP: &str = "make_map";

pub static AS: &str = "as";

pub static ADD: &str = "+";
pub static SUB: &str = "-";
pub static MUL: &str = "*";
pub static DIV: &str = "/";
pub static REM: &str = "%";

pub static EQUAL: &str = "==";
pub static GREATER: &str = ">";
pub static LESS: &str = "<";
pub static GREATER_EQUAL: &str = ">=";
pub static LESS_EQUAL: &str = "<=";

// Constant
pub static MINUS_SIGN: &str = "-";

// Others
pub static IF: &str = "if";
pub static LOOP: &str = "loop";
pub static INFIX: &str = "infix";
pub static INFIXR: &str = "infixr";

pub static IDENTIFIER_NOT_ALLOWED: &[&str] = &[FUNCTION_TYPE_ARROW, FN_ARROW, FN, INFIX, INFIXR];

// Error messages
pub static IDENTIFIER_NOT_DEFINED: &str = "Identifier not defined";
pub static REDEFINITION_NOT_SUPPORTED: &str = "Redefinition not supported";
pub static LVAL_MUST_BE_IDENTIFIER: &str = "lval must be identifier";
pub static TYPE_NOT_MATCHED: &str = "Type not matched";
pub static ARGUMENT_LENGTH_MISMATCH: &str = "Argument length mismatch";
pub static UNABLE_TO_CAST: &str = "Unable to cast";
pub static UNSUPPORTED_OPERATION: &str = "Unsupported operation";
