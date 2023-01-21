//! Provides a structure for the abstract syntax tree (AST) for the Truemoji language.
use std::collections::{HashMap, HashSet};

use rayon::join;
use thiserror::Error;

/// Errors that can occur during evaluation of an AST.
#[derive(Debug, Error, Clone, Copy)]
pub enum EvalErr {
    /// There is an unknown formula in the tree.
    #[error("Truth value of '{0}' could not be determined")]
    UnknownFormula(char),
}

/// A unary operation.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnOp {
    /// The "not" operation.
    Not,
    /// No operation; used for statement-priority reasons.
    Identity,
}

impl UnOp {
    /// Apply a unary operation.
    ///
    /// # Args
    /// * `child` - The term of the operation.
    /// * `model` - The current interpretation of all formulas.
    ///
    /// # Returns
    /// The boolean value of the operation.
    fn apply(&self, child: &Node, model: &HashMap<char, bool>) -> Result<bool, EvalErr> {
        let eval = child.evaluate(model);
        match self {
            Self::Not => eval.map(|b| !b),
            Self::Identity => eval,
        }
    }
}

/// A binary operation.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    /// The "and" operation.
    And,
    /// The "or" operation.
    Or,
    /// The "implies"/"conditional" operation.
    Implies,
    /// The "iff"/"biconditional" operation.
    Iff,
}

impl BinOp {
    /// Apply a binary operation.
    ///
    /// # Args
    /// * `lhs` - The left term of the operation.
    /// * `rhs` - The right term of the operation.
    /// * `model` - The current interpretation of all formulas.
    ///
    /// # Returns
    /// The boolean value of the operation.
    fn apply(&self, lhs: &Node, rhs: &Node, model: &HashMap<char, bool>) -> Result<bool, EvalErr> {
        let (lhs_eval, rhs_eval) = join(|| lhs.evaluate(model), || rhs.evaluate(model));
        let lhs_eval_ok = lhs_eval?;
        let rhs_eval_ok = rhs_eval?;
        Ok(match self {
            Self::And => lhs_eval_ok && rhs_eval_ok,
            Self::Or => lhs_eval_ok || rhs_eval_ok,
            Self::Implies => !lhs_eval_ok || rhs_eval_ok,
            Self::Iff => (!lhs_eval_ok || rhs_eval_ok) && (!rhs_eval_ok || lhs_eval_ok),
        })
    }
}

/// A node in the abstract syntax tree.
#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    /// A boolean value.
    Bool(bool),
    /// A formula.
    Formula(char),
    /// A unary expression.
    UnExpr(UnOp, Box<Node>),
    /// A binary expression.
    BinExpr(BinOp, Box<Node>, Box<Node>),
}

impl Node {
    /// Evaluate a node.
    ///
    /// # Args
    /// * `model` - The current interpretation of all formulas.
    ///
    /// # Returns
    /// The boolean value of the node.
    pub fn evaluate(&self, model: &HashMap<char, bool>) -> Result<bool, EvalErr> {
        match self {
            Self::Bool(b) => Ok(*b),
            Self::Formula(name) => Node::evaluate_formula(name, model),
            Self::UnExpr(op, child) => op.apply(child, model),
            Self::BinExpr(op, lhs, rhs) => op.apply(lhs, rhs, model),
        }
    }

    /// Collect all formula names in the tree.
    ///
    /// # Args
    ///
    /// * `bucket` - A bucket to collect all of the formula names.
    pub fn collect_formulas(&self, bucket: &mut HashSet<char>) {
        match self {
            Self::Bool(_) => {}
            Self::Formula(symbol) => {
                if !bucket.contains(symbol) {
                    bucket.insert(*symbol);
                }
            }
            Self::UnExpr(_, child) => child.collect_formulas(bucket),
            Self::BinExpr(_, lhs, rhs) => {
                let mut lhs_bucket = HashSet::new();
                let mut rhs_bucket = HashSet::new();
                join(
                    || lhs.collect_formulas(&mut lhs_bucket),
                    || rhs.collect_formulas(&mut rhs_bucket),
                );
                bucket.extend(lhs_bucket);
                bucket.extend(rhs_bucket);
            }
        }
    }

    /// Determine the boolean value of a formula.
    ///
    /// # Args
    /// * `name` - The name of the formula.
    /// * `model` - The current interpretation of all formulas.
    ///
    /// # Returns
    /// The boolean value of the formula.
    fn evaluate_formula(name: &char, model: &HashMap<char, bool>) -> Result<bool, EvalErr> {
        model
            .get(name)
            .map(|b| *b)
            .ok_or_else(|| EvalErr::UnknownFormula(*name))
    }
}

#[cfg(test)]
mod tests {
    use rstest::*;

    use super::*;

    #[fixture]
    fn model() -> HashMap<char, bool> {
        let mut model = HashMap::new();
        model.insert('a', true);
        model.insert('b', false);
        model
    }

    #[rstest]
    fn test_eval_err_display(#[values('a', '0', '🦀')] formula: char) {
        assert_eq!(
            format!("Truth value of '{}' could not be determined", formula),
            format!("{}", EvalErr::UnknownFormula(formula)),
        );
    }

    #[rstest]
    #[case(UnOp::Not, Node::Bool(true), false)]
    #[case(UnOp::Identity, Node::Bool(true), true)]
    fn test_un_op_ok(
        #[case] op: UnOp,
        #[case] child: Node,
        #[case] expected: bool,
        model: HashMap<char, bool>,
    ) {
        assert_eq!(op.apply(&child, &model).unwrap(), expected);
    }

    #[rstest]
    fn test_un_op_err(model: HashMap<char, bool>) {
        assert!(UnOp::Identity.apply(&Node::Formula('c'), &model).is_err());
    }

    #[rstest]
    #[case(BinOp::And, Node::Bool(false), Node::Bool(false), false)]
    #[case(BinOp::And, Node::Bool(false), Node::Bool(true), false)]
    #[case(BinOp::And, Node::Bool(true), Node::Bool(false), false)]
    #[case(BinOp::And, Node::Bool(true), Node::Bool(true), true)]
    #[case(BinOp::Or, Node::Bool(false), Node::Bool(false), false)]
    #[case(BinOp::Or, Node::Bool(false), Node::Bool(true), true)]
    #[case(BinOp::Or, Node::Bool(true), Node::Bool(false), true)]
    #[case(BinOp::Or, Node::Bool(true), Node::Bool(true), true)]
    #[case(BinOp::Implies, Node::Bool(false), Node::Bool(false), true)]
    #[case(BinOp::Implies, Node::Bool(false), Node::Bool(true), true)]
    #[case(BinOp::Implies, Node::Bool(true), Node::Bool(false), false)]
    #[case(BinOp::Implies, Node::Bool(true), Node::Bool(true), true)]
    #[case(BinOp::Iff, Node::Bool(false), Node::Bool(false), true)]
    #[case(BinOp::Iff, Node::Bool(false), Node::Bool(true), false)]
    #[case(BinOp::Iff, Node::Bool(true), Node::Bool(false), false)]
    #[case(BinOp::Iff, Node::Bool(true), Node::Bool(true), true)]
    fn test_bin_op_ok(
        #[case] op: BinOp,
        #[case] lhs: Node,
        #[case] rhs: Node,
        #[case] expected: bool,
        model: HashMap<char, bool>,
    ) {
        assert_eq!(op.apply(&lhs, &rhs, &model).unwrap(), expected);
    }

    #[rstest]
    fn test_bin_op_err(model: HashMap<char, bool>) {
        assert!(BinOp::And
            .apply(&Node::Bool(true), &Node::Formula('c'), &model)
            .is_err());
    }

    #[rstest]
    #[case('a', true)]
    #[case('b', false)]
    fn test_node_evaluate_formula_ok(
        #[case] name: char,
        #[case] expected: bool,
        model: HashMap<char, bool>,
    ) {
        assert_eq!(Node::evaluate_formula(&name, &model).unwrap(), expected);
    }

    #[rstest]
    fn test_node_evaluate_formula_err(model: HashMap<char, bool>) {
        assert!(Node::evaluate_formula(&'c', &model).is_err());
    }

    #[rstest]
    #[case(Node::Bool(true), HashSet::new())]
    #[case(Node::Formula('a'), HashSet::from(['a']))]
    #[case(Node::UnExpr(UnOp::Identity, Box::new(Node::Formula('b'))), HashSet::from(['b']))]
    #[case(
        Node::BinExpr(
            BinOp::Iff,
            Box::new(
                Node::BinExpr(BinOp::Iff,
                    Box::new(Node::Formula('a')),
                    Box::new(Node::Formula('b')))),
            Box::new(Node::Formula('a'))),
        HashSet::from(['a', 'b']),
    )]
    fn test_node_collect_formulas(#[case] node: Node, #[case] expected: HashSet<char>) {
        let mut bucket = HashSet::new();
        node.collect_formulas(&mut bucket);
        assert_eq!(bucket, expected);
    }

    #[rstest]
    #[case(Node::Bool(true), true)]
    #[case(Node::Formula('b'), false)]
    #[case(Node::UnExpr(UnOp::Not, Box::new(Node::Bool(true))), false)]
    #[case(
        Node::BinExpr(BinOp::And, Box::new(Node::Bool(true)), Box::new(Node::Bool(true))),
        true
    )]
    fn test_node_evaluate_ok(
        #[case] node: Node,
        #[case] expected: bool,
        model: HashMap<char, bool>,
    ) {
        assert_eq!(node.evaluate(&model).unwrap(), expected);
    }

    #[rstest]
    fn test_node_evaluate_err(model: HashMap<char, bool>) {
        assert!(Node::Formula('c').evaluate(&model).is_err());
    }
}
