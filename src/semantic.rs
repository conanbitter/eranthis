use std::collections::HashMap;

use crate::ast::{CodeBlock, ExprNodeData, ExprType};

enum ValueType {
    Byte,
    Int,
    Float,
    Fixed,
    String,
    Bool,
}

enum Value {
    Byte(u8),
    Int(i32),
    Float(f32),
    Fixed(f32),
    String(u32),
    Bool(bool),
}

enum Operand {
    Static(u32),
    Register(u32),
}

struct LocalVar {
    position: usize,
    vartype: ValueType,
}

struct GlobalVar {
    offset: u32,
    valtype: ValueType,
    init: Value,
}

struct Constant {
    valtype: ValueType,
    value: Value,
}

struct Context {
    vars: HashMap<String, LocalVar>,
    parent: u32,
}

struct Function {
    params: Vec<(String, ValueType)>,
    return_type: ValueType,
    context_id: usize,
    body: CodeBlock,
}
struct Module {
    constants: HashMap<String, Constant>,
    global_vars: HashMap<String, GlobalVar>,
    strings: Vec<String>,
    contexts: Vec<Context>,
    functions: HashMap<String, Function>,
}

impl Module {
    pub fn new() -> Module {
        Module {
            constants: HashMap::new(),
            global_vars: HashMap::new(),
            strings: Vec::new(),
            contexts: Vec::new(),
            functions: HashMap::new(),
        }
    }

    pub fn get_constant(&self, name: &String) -> Option<&Constant> {
        self.constants.get(name)
    }
}
