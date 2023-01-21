//! Provides functionality to convert Truemoji language tokens into an abstract syntax tree (AST).
use thiserror::Error;

use crate::{
    ast::{BinOp, Node, UnOp},
    lex::Token,
};

/// Errors that can occur during the parsing of tokens.
#[derive(Debug, Copy, Clone, Error)]
pub enum ParseErr {
    /// Unexpected end of token input.
    #[error("Unexpected end of input at token position {0}")]
    EndOfInput(usize),
    /// An unexpected token occured.
    #[error("Unexpected token {0} at token position {1}")]
    UnexpectedToken(Token, usize),
}

/// Parse Truemoji language tokens.
///
/// # Args
/// * `tokens` - The input tokens.
///
/// # Returns
/// The AST that represents the tokens.
pub fn parse(tokens: &[Token]) -> Result<Node, ParseErr> {
    let (expr, final_pos) = parse_iff(tokens, 0)?;
    if final_pos == tokens.len() {
        Ok(expr)
    } else {
        Err(ParseErr::UnexpectedToken(tokens[final_pos], final_pos))
    }
}

/// Parse an "iff" expression.
///
/// # Args
/// * `tokens` - The input tokens.
/// * `pos` - The current position within the tokens.
///
/// # Returns
/// The AST that represents the expression and the next position to continue parsing.
fn parse_iff(tokens: &[Token], pos: usize) -> Result<(Node, usize), ParseErr> {
    parse_bin_op(tokens, pos, BinOp::Iff, parse_implies)
}

/// Parse an "implies" expression.
///
/// # Args
/// * `tokens` - The input tokens.
/// * `pos` - The current position within the tokens.
///
/// # Returns
/// The AST that represents the expression and the next position to continue parsing.
fn parse_implies(tokens: &[Token], pos: usize) -> Result<(Node, usize), ParseErr> {
    parse_bin_op(tokens, pos, BinOp::Implies, parse_or)
}

/// Parse an "or" expression.
///
/// # Args
/// * `tokens` - The input tokens.
/// * `pos` - The current position within the tokens.
///
/// # Returns
/// The AST that represents the expression and the next position to continue parsing.
fn parse_or(tokens: &[Token], pos: usize) -> Result<(Node, usize), ParseErr> {
    parse_bin_op(tokens, pos, BinOp::Or, parse_and)
}

/// Parse an "and" expression.
///
/// # Args
/// * `tokens` - The input tokens.
/// * `pos` - The current position within the tokens.
///
/// # Returns
/// The AST that represents the expression and the next position to continue parsing.
fn parse_and(tokens: &[Token], pos: usize) -> Result<(Node, usize), ParseErr> {
    parse_bin_op(tokens, pos, BinOp::And, parse_not)
}

/// Parse a "not" expression.
///
/// # Args
/// * `tokens` - The input tokens.
/// * `pos` - The current position within the tokens.
///
/// # Returns
/// The AST that represents the expression and the next position to continue parsing.
fn parse_not(tokens: &[Token], pos: usize) -> Result<(Node, usize), ParseErr> {
    parse_un_op(tokens, pos, UnOp::Not, parse_formula)
}

/// Parse an "formula" expression.
///
/// # Args
/// * `tokens` - The input tokens.
/// * `pos` - The current position within the tokens.
///
/// # Returns
/// The AST that represents the expression and the next position to continue parsing.
fn parse_formula(tokens: &[Token], pos: usize) -> Result<(Node, usize), ParseErr> {
    let next = tokens.get(pos).ok_or(ParseErr::EndOfInput(pos))?;
    match next {
        &Token::True => Ok((Node::Bool(true), pos + 1)),
        &Token::False => Ok((Node::Bool(false), pos + 1)),
        &Token::Formula(f) => Ok((Node::Formula(f), pos + 1)),
        &Token::OpenParen => {
            let (child, next_pos) = parse_iff(tokens, pos + 1)?;
            let match_paren = tokens.get(next_pos);
            match match_paren {
                Some(&Token::CloseParen) => {
                    Ok((Node::UnExpr(UnOp::Identity, Box::new(child)), next_pos + 1))
                }
                _ => Err(ParseErr::UnexpectedToken(Token::CloseParen, next_pos)),
            }
        }
        _ => Err(ParseErr::UnexpectedToken(*next, pos)),
    }
}

/// Parse an expression that represents a unary operation.
///
/// # Args
/// * `tokens` - The input tokens.
/// * `pos` - The current position within the tokens.
/// * `op` - The operation token to check for.
/// * `descent` - The parse function to call when descending down the syntax tree.
///
/// # Returns
/// The AST that represents the expression and the next position to continue parsing.
fn parse_un_op(
    tokens: &[Token],
    pos: usize,
    op: UnOp,
    descent: fn(&[Token], usize) -> Result<(Node, usize), ParseErr>,
) -> Result<(Node, usize), ParseErr> {
    let next = tokens.get(pos);
    match next {
        Some(&Token::UnOp(true_op)) if true_op == op => {
            let (child, next_pos) = descent(tokens, pos + 1)?;
            Ok((Node::UnExpr(op, Box::new(child)), next_pos))
        }
        _ => descent(tokens, pos),
    }
}

/// Parse an expression that represents a binary operation.
///
/// # Args
/// * `tokens` - The input tokens.
/// * `pos` - The current position within the tokens.
/// * `op` - The operation token to check for.
/// * `descent` - The parse function to call when descending down the syntax tree.
///
/// # Returns
/// The AST that represents the expression and the next position to continue parsing.
fn parse_bin_op(
    tokens: &[Token],
    pos: usize,
    op: BinOp,
    descent: fn(&[Token], usize) -> Result<(Node, usize), ParseErr>,
) -> Result<(Node, usize), ParseErr> {
    let (lhs, next_pos) = descent(tokens, pos)?;
    let next = tokens.get(next_pos);
    match next {
        Some(&Token::BinOp(true_op)) if true_op == op => {
            let (rhs, final_pos) = parse_bin_op(tokens, next_pos + 1, op, descent)?;
            Ok((Node::BinExpr(op, Box::new(lhs), Box::new(rhs)), final_pos))
        }
        _ => Ok((lhs, next_pos)),
    }
}

#[cfg(test)]
mod tests {
    use rstest::*;

    use super::*;

    #[rstest]
    #[case(
        ParseErr::EndOfInput(12),
        "Unexpected end of input at token position 12"
    )]
    #[case(
        ParseErr::UnexpectedToken(Token::Formula('a'), 12),
        "Unexpected token a at token position 12"
    )]
    fn test_parse_err_display(#[case] err: ParseErr, #[case] expected: String) {
        assert_eq!(format!("{}", err), expected);
    }

    #[rstest]
    #[case(&[Token::True], Node::Bool(true))]
    #[case(&[Token::Formula('a')], Node::Formula('a'))]
    #[case(
        &[Token::OpenParen, Token::False, Token::CloseParen],
        Node::UnExpr(UnOp::Identity, Box::new(Node::Bool(false)))
    )]
    #[case(
        &[Token::UnOp(UnOp::Not), Token::False],
        Node::UnExpr(UnOp::Not, Box::new(Node::Bool(false)))
    )]
    #[case(
        &[Token::True, Token::BinOp(BinOp::And), Token::False],
        Node::BinExpr(BinOp::And, Box::new(Node::Bool(true)), Box::new(Node::Bool(false)))
    )]
    #[case(
        &[Token::True, Token::BinOp(BinOp::Or), Token::False],
        Node::BinExpr(BinOp::Or, Box::new(Node::Bool(true)), Box::new(Node::Bool(false)))
    )]
    #[case(
        &[Token::True, Token::BinOp(BinOp::Implies), Token::False],
        Node::BinExpr(BinOp::Implies, Box::new(Node::Bool(true)), Box::new(Node::Bool(false)))
    )]
    #[case(
        &[Token::True, Token::BinOp(BinOp::Iff), Token::False],
        Node::BinExpr(BinOp::Iff, Box::new(Node::Bool(true)), Box::new(Node::Bool(false)))
    )]
    #[case(
        &[Token::True, Token::BinOp(BinOp::Iff), Token::False, Token::BinOp(BinOp::And), Token::True],
        Node::BinExpr(
            BinOp::Iff,
            Box::new(Node::Bool(true)),
            Box::new(Node::BinExpr(BinOp::And, Box::new(Node::Bool(false)), Box::new(Node::Bool(true)))))
    )]
    fn test_parse_ok(#[case] tokens: &[Token], #[case] expected: Node) {
        assert_eq!(parse(tokens).unwrap(), expected);
    }

    #[rstest]
    #[case(&[Token::OpenParen])]
    #[case(&[Token::Formula('a'), Token::True])]
    #[case(&[Token::UnOp(UnOp::Not)])]
    #[case(&[Token::True, Token::BinOp(BinOp::And)])]
    #[case(&[Token::BinOp(BinOp::And), Token::True])]
    fn test_parse_err(#[case] tokens: &[Token]) {
        assert!(parse(tokens).is_err());
    }
}
