use std::collections::HashMap;

use anyhow::bail;

use crate::{
    ast::{BinOp, CodeBlock, DataType, ExprNode, ModNode, ModNodeData, UnOp},
    fixedpoint::FixedPoint,
    lexer::FilePos,
};

#[derive(Debug)]
enum ValueType {
    Byte,
    Int,
    Float,
    Fixed,
    String,
    Bool,
}

#[derive(Debug, Clone)]
pub enum Value {
    Byte(u8),
    Int(i64),
    Float(f64),
    Fixed(FixedPoint),
    String(String),
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

#[derive(Debug)]
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
pub struct Module {
    constants: HashMap<String, Constant>,
    global_vars: HashMap<String, GlobalVar>,
    strings: Vec<String>,
    contexts: Vec<Context>,
    functions: HashMap<String, Function>,
}

pub enum ResolveResult {
    Success(Value, bool),  // value, is literal
    Fail(String, FilePos), // name, pos
}

fn get_common_type(
    (left_value, left_literal): (&Value, bool),
    (right_value, right_literal): (&Value, bool),
) -> Option<DataType> {
    match left_value {
        Value::Byte(_) => match right_value {
            Value::Byte(_) => Some(DataType::Byte),
            Value::Int(_) => {
                if right_literal {
                    Some(DataType::Byte)
                } else {
                    None
                }
            }
            _ => None,
        },
        Value::Int(_) => match right_value {
            Value::Byte(_) => {
                if left_literal {
                    Some(DataType::Byte)
                } else {
                    None
                }
            }
            Value::Int(_) => Some(DataType::Int),
            Value::Float(_) => {
                if left_literal {
                    Some(DataType::Float)
                } else {
                    None
                }
            }
            Value::Fixed(_) => {
                if left_literal {
                    Some(DataType::Float)
                } else {
                    None
                }
            }
            _ => None,
        },
        Value::Float(_) => match right_value {
            Value::Int(_) => {
                if right_literal {
                    Some(DataType::Float)
                } else {
                    None
                }
            }
            Value::Float(_) => Some(DataType::Float),
            Value::Fixed(_) => {
                if left_literal {
                    Some(DataType::Fixed)
                } else {
                    None
                }
            }
            _ => None,
        },
        Value::Fixed(_) => match right_value {
            Value::Int(_) => {
                if right_literal {
                    Some(DataType::Fixed)
                } else {
                    None
                }
            }
            Value::Float(_) => {
                if right_literal {
                    Some(DataType::Fixed)
                } else {
                    None
                }
            }
            Value::Fixed(_) => Some(DataType::Fixed),
            _ => None,
        },
        Value::String(_) => match right_value {
            Value::String(_) => Some(DataType::Fixed),
            _ => None,
        },
        Value::Bool(_) => match right_value {
            Value::Bool(_) => Some(DataType::Bool),
            _ => None,
        },
    }
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

    pub fn collect_constants(&mut self, root: &mut Vec<ModNode>) -> anyhow::Result<()> {
        let mut constants: HashMap<String, &mut ExprNode> = HashMap::new();
        for item in root {
            if let ModNode {
                data: ModNodeData::ConstDecl(list),
                ..
            } = item
            {
                for (name, _, value, _) in list {
                    constants.insert(name.clone(), value);
                }
            }
        }

        let mut resolved: Vec<String> = vec![];

        while !constants.is_empty() {
            resolved.clear();
            for (name, expr) in &constants {
                match self.constexpr_resolve(expr)? {
                    ResolveResult::Success(value, _) => {
                        self.constants.insert(
                            name.clone(),
                            Constant {
                                value,
                                valtype: ValueType::Bool,
                            },
                        );
                        resolved.push(name.clone());
                    }
                    ResolveResult::Fail(_, _) => {}
                }
            }
            if resolved.is_empty() {
                bail!(
                    "Cyclic constexpr resolve, constants: {}",
                    constants.keys().cloned().collect::<Vec<String>>().join(", ")
                )
            }
            for name in &resolved {
                constants.remove(name);
            }
        }

        println!("{:?} \n\n {:?}", self.constants, &constants);

        Ok(())
    }

    pub fn constexpr_resolve(&self, node: &ExprNode) -> anyhow::Result<ResolveResult> {
        match &node.data {
            crate::ast::ExprNodeData::IntLiteral(v) => Ok(ResolveResult::Success(Value::Int(*v), true)),
            crate::ast::ExprNodeData::FloatLiteral(v) => Ok(ResolveResult::Success(Value::Float(*v), true)),
            crate::ast::ExprNodeData::StringLiteral(v) => Ok(ResolveResult::Success(Value::String(v.clone()), true)),
            crate::ast::ExprNodeData::BoolLiteral(v) => Ok(ResolveResult::Success(Value::Bool(*v), true)),
            crate::ast::ExprNodeData::BinOp(op, expr_left, expr_right) => {
                self.constexpr_binop(expr_left, expr_right, *op)
            }
            crate::ast::ExprNodeData::UnOp(op, expr) => self.constexpr_unop(expr, *op),
            crate::ast::ExprNodeData::Var(items) => {
                if items.len() > 1 {
                    anyhow::bail!("error {}", line!())
                }
                if let Some(Constant { value, .. }) = self.constants.get(&items[0]) {
                    Ok(ResolveResult::Success(value.clone(), false))
                } else {
                    Ok(ResolveResult::Fail(items[0].clone(), node.pos))
                }
            }
            crate::ast::ExprNodeData::FnCall(_, _) => anyhow::bail!("error {}", line!()),
            crate::ast::ExprNodeData::TypeConvert(expr_node, data_type) => {
                let value = self.constexpr_resolve(expr_node)?;
                match value {
                    ResolveResult::Fail(_, _) => Ok(value),
                    ResolveResult::Success(value, _) => self
                        .constexpr_typeconvert(value, *data_type)
                        .map(|res| ResolveResult::Success(res, false)),
                }
            }
            crate::ast::ExprNodeData::Subscript(_, _) => anyhow::bail!("error {}", line!()),
        }
    }

    fn constexpr_typeconvert(&self, value: Value, new_type: DataType) -> anyhow::Result<Value> {
        match new_type {
            DataType::Byte => match value {
                Value::Byte(_) => Ok(value),
                Value::Int(v) => Ok(Value::Byte(v as u8)),
                Value::Float(v) => Ok(Value::Byte(v as u8)),
                Value::Fixed(v) => Ok(Value::Byte(i32::from(v) as u8)),
                _ => anyhow::bail!("error {}", line!()),
            },

            DataType::Int => match value {
                Value::Byte(v) => Ok(Value::Int(v as i64)),
                Value::Int(_) => Ok(value),
                Value::Float(v) => Ok(Value::Int(v as i64)),
                Value::Fixed(v) => Ok(Value::Int(i32::from(v) as i64)),
                _ => anyhow::bail!("error {}", line!()),
            },

            DataType::Float => match value {
                Value::Byte(v) => Ok(Value::Float(v as f64)),
                Value::Int(v) => Ok(Value::Float(v as f64)),
                Value::Float(_) => Ok(value),
                Value::Fixed(v) => Ok(Value::Float(f64::from(v))),
                _ => anyhow::bail!("error {}", line!()),
            },

            DataType::Fixed => match value {
                Value::Byte(v) => Ok(Value::Fixed(FixedPoint::from(v as i32))),
                Value::Int(v) => Ok(Value::Fixed(FixedPoint::from(v))),
                Value::Float(v) => Ok(Value::Fixed(FixedPoint::from(v))),
                Value::Fixed(_) => Ok(value),
                _ => anyhow::bail!("error {}", line!()),
            },

            DataType::String => match value {
                Value::Byte(v) => Ok(Value::String(format!("{}", v))),
                Value::Int(v) => Ok(Value::String(format!("{}", v))),
                Value::Float(v) => Ok(Value::String(format!("{}", v))),
                Value::Fixed(v) => Ok(Value::String(format!("{}", v))),
                Value::String(_) => Ok(value),
                Value::Bool(v) => {
                    if v {
                        Ok(Value::String(String::from("true")))
                    } else {
                        Ok(Value::String(String::from("false")))
                    }
                }
            },

            DataType::Bool => match value {
                Value::Bool(_) => Ok(value),
                _ => anyhow::bail!("error {}", line!()),
            },
        }
    }

    fn constexpr_binop(&self, left: &ExprNode, right: &ExprNode, op: BinOp) -> anyhow::Result<ResolveResult> {
        let left = self.constexpr_resolve(left)?;
        match left {
            ResolveResult::Fail(_, _) => Ok(left),
            ResolveResult::Success(left, left_literal) => {
                let right = self.constexpr_resolve(right)?;
                match right {
                    ResolveResult::Fail(_, _) => Ok(right),
                    ResolveResult::Success(right, right_literal) => {
                        if let Some(common_type) = get_common_type((&left, left_literal), (&right, right_literal)) {
                            let left = self.constexpr_typeconvert(left, common_type)?;
                            let right = self.constexpr_typeconvert(right, common_type)?;
                            match op {
                                BinOp::Add => match (left, right) {
                                    (Value::Byte(lv), Value::Byte(rv)) => {
                                        Ok(ResolveResult::Success(Value::Byte(lv + rv), false))
                                    }
                                    (Value::Int(lv), Value::Int(rv)) => {
                                        Ok(ResolveResult::Success(Value::Int(lv + rv), false))
                                    }
                                    (Value::Float(lv), Value::Float(rv)) => {
                                        Ok(ResolveResult::Success(Value::Float(lv + rv), false))
                                    }
                                    (Value::Fixed(lv), Value::Fixed(rv)) => {
                                        Ok(ResolveResult::Success(Value::Fixed(lv + rv), false))
                                    }
                                    (Value::String(lv), Value::String(rv)) => {
                                        Ok(ResolveResult::Success(Value::String(lv + rv.as_str()), false))
                                    }
                                    _ => bail!("error {}", line!()),
                                },
                                BinOp::Sub => match (left, right) {
                                    (Value::Byte(lv), Value::Byte(rv)) => {
                                        Ok(ResolveResult::Success(Value::Byte(lv - rv), false))
                                    }
                                    (Value::Int(lv), Value::Int(rv)) => {
                                        Ok(ResolveResult::Success(Value::Int(lv - rv), false))
                                    }
                                    (Value::Float(lv), Value::Float(rv)) => {
                                        Ok(ResolveResult::Success(Value::Float(lv - rv), false))
                                    }
                                    (Value::Fixed(lv), Value::Fixed(rv)) => {
                                        Ok(ResolveResult::Success(Value::Fixed(lv - rv), false))
                                    }
                                    _ => bail!("error {}", line!()),
                                },
                                BinOp::Mul => match (left, right) {
                                    (Value::Byte(lv), Value::Byte(rv)) => {
                                        Ok(ResolveResult::Success(Value::Byte(lv * rv), false))
                                    }
                                    (Value::Int(lv), Value::Int(rv)) => {
                                        Ok(ResolveResult::Success(Value::Int(lv * rv), false))
                                    }
                                    (Value::Float(lv), Value::Float(rv)) => {
                                        Ok(ResolveResult::Success(Value::Float(lv * rv), false))
                                    }
                                    (Value::Fixed(lv), Value::Fixed(rv)) => {
                                        Ok(ResolveResult::Success(Value::Fixed(lv * rv), false))
                                    }
                                    _ => bail!("error {}", line!()),
                                },
                                BinOp::Div => match (left, right) {
                                    (Value::Byte(lv), Value::Byte(rv)) => {
                                        Ok(ResolveResult::Success(Value::Byte(lv / rv), false))
                                    }
                                    (Value::Int(lv), Value::Int(rv)) => {
                                        Ok(ResolveResult::Success(Value::Int(lv / rv), false))
                                    }
                                    (Value::Float(lv), Value::Float(rv)) => {
                                        Ok(ResolveResult::Success(Value::Float(lv / rv), false))
                                    }
                                    (Value::Fixed(lv), Value::Fixed(rv)) => {
                                        Ok(ResolveResult::Success(Value::Fixed(lv / rv), false))
                                    }
                                    _ => bail!("error {}", line!()),
                                },
                                BinOp::Mod => match (left, right) {
                                    (Value::Byte(lv), Value::Byte(rv)) => {
                                        Ok(ResolveResult::Success(Value::Byte(lv % rv), false))
                                    }
                                    (Value::Int(lv), Value::Int(rv)) => {
                                        Ok(ResolveResult::Success(Value::Int(lv % rv), false))
                                    }
                                    _ => bail!("error {}", line!()),
                                },
                                BinOp::Less => match (left, right) {
                                    (Value::Byte(lv), Value::Byte(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv < rv), false))
                                    }
                                    (Value::Int(lv), Value::Int(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv < rv), false))
                                    }
                                    (Value::Float(lv), Value::Float(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv < rv), false))
                                    }
                                    (Value::Fixed(lv), Value::Fixed(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv < rv), false))
                                    }
                                    _ => bail!("error {}", line!()),
                                },
                                BinOp::LessEq => match (left, right) {
                                    (Value::Byte(lv), Value::Byte(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv <= rv), false))
                                    }
                                    (Value::Int(lv), Value::Int(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv <= rv), false))
                                    }
                                    (Value::Float(lv), Value::Float(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv <= rv), false))
                                    }
                                    (Value::Fixed(lv), Value::Fixed(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv <= rv), false))
                                    }
                                    _ => bail!("error {}", line!()),
                                },
                                BinOp::Greater => match (left, right) {
                                    (Value::Byte(lv), Value::Byte(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv > rv), false))
                                    }
                                    (Value::Int(lv), Value::Int(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv > rv), false))
                                    }
                                    (Value::Float(lv), Value::Float(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv > rv), false))
                                    }
                                    (Value::Fixed(lv), Value::Fixed(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv > rv), false))
                                    }
                                    _ => bail!("error {}", line!()),
                                },
                                BinOp::GreaterEq => match (left, right) {
                                    (Value::Byte(lv), Value::Byte(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv >= rv), false))
                                    }
                                    (Value::Int(lv), Value::Int(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv >= rv), false))
                                    }
                                    (Value::Float(lv), Value::Float(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv >= rv), false))
                                    }
                                    (Value::Fixed(lv), Value::Fixed(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv >= rv), false))
                                    }
                                    _ => bail!("error {}", line!()),
                                },
                                BinOp::Eq => match (left, right) {
                                    (Value::Byte(lv), Value::Byte(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv == rv), false))
                                    }
                                    (Value::Int(lv), Value::Int(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv == rv), false))
                                    }
                                    (Value::Float(lv), Value::Float(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv == rv), false))
                                    }
                                    (Value::Fixed(lv), Value::Fixed(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv == rv), false))
                                    }
                                    _ => bail!("error {}", line!()),
                                },
                                BinOp::NotEq => match (left, right) {
                                    (Value::Byte(lv), Value::Byte(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv != rv), false))
                                    }
                                    (Value::Int(lv), Value::Int(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv != rv), false))
                                    }
                                    (Value::Float(lv), Value::Float(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv != rv), false))
                                    }
                                    (Value::Fixed(lv), Value::Fixed(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv != rv), false))
                                    }
                                    _ => bail!("error {}", line!()),
                                },
                                BinOp::And => match (left, right) {
                                    (Value::Bool(lv), Value::Bool(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv && rv), false))
                                    }
                                    _ => bail!("error {}", line!()),
                                },
                                BinOp::Or => match (left, right) {
                                    (Value::Bool(lv), Value::Bool(rv)) => {
                                        Ok(ResolveResult::Success(Value::Bool(lv || rv), false))
                                    }
                                    _ => bail!("error {}", line!()),
                                },
                            }
                        } else {
                            bail!("error {}", line!());
                        }
                    }
                }
            }
        }
    }

    fn constexpr_unop(&self, expr: &ExprNode, op: UnOp) -> anyhow::Result<ResolveResult> {
        let expr = self.constexpr_resolve(expr)?;
        match expr {
            ResolveResult::Fail(_, _) => Ok(expr),
            ResolveResult::Success(value, _) => match op {
                UnOp::Neg => match value {
                    Value::Int(v) => Ok(ResolveResult::Success(Value::Int(-v), false)),
                    Value::Float(v) => Ok(ResolveResult::Success(Value::Float(-v), false)),
                    Value::Fixed(v) => Ok(ResolveResult::Success(Value::Fixed(-v), false)),
                    _ => bail!("error {}", line!()),
                },
                UnOp::Not => match value {
                    Value::Bool(v) => Ok(ResolveResult::Success(Value::Bool(!v), false)),
                    _ => bail!("error {}", line!()),
                },
            },
        }
    }

    pub fn debug_print(&self) {
        //println!("{:?}", self.constants);
    }
}
