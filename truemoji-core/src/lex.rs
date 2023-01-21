//! Provides functionality to lex input into Truemoji language tokens.
use std::fmt::Display;

use rayon::prelude::*;
use thiserror::Error;

use crate::ast::{BinOp, UnOp};

/// Character for open parentheses.
pub const OPEN_PAREN: char = '😮';
/// Character for closed parentheses.
pub const CLOSE_PAREN: char = '😶';
/// Character for "and" operator.
pub const AND: char = '👏';
/// Character for "or" operator.
pub const OR: char = '🙌';
/// Character for "not" operator.
pub const NOT: char = '🚫';
/// Character for "implies"/"conditional" operator.
pub const IMPLIES: char = '👉';
/// Character for "iff"/"biconditional" operator.
pub const IFF: char = '🤝';
/// Character for "true" boolean value.
pub const TRUE: char = '👍';
/// Character for "false" boolean value.
pub const FALSE: char = '👎';
/// Character for "identity" operator. Not a part of the language spec, but is here for completeness and banter.
pub const IDEN: char = '🦀';

impl TryFrom<char> for BinOp {
    type Error = char;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            AND => Ok(BinOp::And),
            OR => Ok(BinOp::Or),
            IMPLIES => Ok(BinOp::Implies),
            IFF => Ok(BinOp::Iff),
            _ => Err(value),
        }
    }
}

impl From<BinOp> for char {
    fn from(value: BinOp) -> Self {
        match value {
            BinOp::And => AND,
            BinOp::Or => OR,
            BinOp::Implies => IMPLIES,
            BinOp::Iff => IFF,
        }
    }
}

impl TryFrom<char> for UnOp {
    type Error = char;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            NOT => Ok(UnOp::Not),
            _ => Err(value),
        }
    }
}

impl From<UnOp> for char {
    fn from(value: UnOp) -> Self {
        match value {
            UnOp::Not => NOT,
            UnOp::Identity => IDEN,
        }
    }
}

/// Errors that can occur during the lexing of input.
#[derive(Debug, Clone, Copy, Error)]
pub enum LexErr {
    /// An illegal character is present at some position in the input.
    #[error("Illegal character '{1}' at position {0}")]
    IllegalChar(usize, char),
}

/// A language token.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token {
    /// Open parentheses token.
    OpenParen,
    /// Closed parentheses token.
    CloseParen,
    /// Unary operation token.
    UnOp(UnOp),
    /// Binary operation token.
    BinOp(BinOp),
    /// True token.
    True,
    /// False token.
    False,
    /// Formula token.
    Formula(char),
}

impl TryFrom<char> for Token {
    type Error = char;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            OPEN_PAREN => Ok(Token::OpenParen),
            CLOSE_PAREN => Ok(Token::CloseParen),
            AND | OR | IMPLIES | IFF => Ok(Token::BinOp(BinOp::try_from(value)?)),
            NOT => Ok(Token::UnOp(UnOp::try_from(value)?)),
            TRUE => Ok(Token::True),
            FALSE => Ok(Token::False),
            _ => {
                if value.is_alphanumeric() {
                    Ok(Token::Formula(value))
                } else {
                    Err(value)
                }
            }
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let c = match self {
            Self::OpenParen => OPEN_PAREN,
            Self::CloseParen => CLOSE_PAREN,
            Self::True => TRUE,
            Self::False => FALSE,
            Self::Formula(f) => *f,
            Self::BinOp(op) => char::from(*op),
            Self::UnOp(op) => char::from(*op),
        };
        write!(f, "{}", c)
    }
}

/// Lex tokens from string input.
///
/// > **Note**: this function will automatically filter out whitespace,
/// > which isn't necessarily defined in the language specification
/// > but is nice to have anyways.
///
/// # Args
/// * `input` - The string input.
///
/// # Returns
/// * The tokens from the input.
pub fn lex(input: &str) -> Result<Vec<Token>, LexErr> {
    let chars: Vec<char> = input.par_chars().filter(|c| !c.is_whitespace()).collect();
    let tokens: Result<Vec<Token>, LexErr> = chars
        .into_par_iter()
        .enumerate()
        .map(|(i, c)| Token::try_from(c).map_err(|e| LexErr::IllegalChar(i, e)))
        .collect();
    tokens
}

#[cfg(test)]
mod tests {
    use rstest::*;

    use super::*;

    #[rstest]
    #[case('🚫', UnOp::Not)]
    fn test_un_op_try_from_char_ok(#[case] c: char, #[case] expected: UnOp) {
        assert_eq!(UnOp::try_from(c).unwrap(), expected);
    }

    #[rstest]
    fn test_un_op_try_from_char_err() {
        assert!(UnOp::try_from('👏').is_err());
    }

    #[rstest]
    #[case('👏', BinOp::And)]
    #[case('🙌', BinOp::Or)]
    #[case('👉', BinOp::Implies)]
    #[case('🤝', BinOp::Iff)]
    fn test_bin_op_try_from_char_ok(#[case] c: char, #[case] expected: BinOp) {
        assert_eq!(BinOp::try_from(c).unwrap(), expected);
    }

    #[rstest]
    fn test_bin_op_try_from_char_err() {
        assert!(BinOp::try_from('🚫').is_err());
    }

    #[rstest]
    fn test_lex_err_display(
        #[values('a', '0', '🦀')] c: char,
        #[values(usize::MIN, 0, usize::MAX)] pos: usize,
    ) {
        assert_eq!(
            format!("Illegal character '{}' at position {}", c, pos),
            format!("{}", LexErr::IllegalChar(pos, c))
        );
    }

    #[rstest]
    #[case(Token::OpenParen, "😮")]
    #[case(Token::CloseParen, "😶")]
    #[case(Token::UnOp(UnOp::Not), "🚫")]
    #[case(Token::BinOp(BinOp::And), "👏")]
    #[case(Token::True, "👍")]
    #[case(Token::False, "👎")]
    #[case(Token::Formula('a'), "a")]
    fn test_token_display(#[case] token: Token, #[case] expected: String) {
        assert_eq!(format!("{}", token), expected);
    }

    #[rstest]
    #[case('😮', Token::OpenParen)]
    #[case('😶', Token::CloseParen)]
    #[case('🚫', Token::UnOp(UnOp::Not))]
    #[case('👏', Token::BinOp(BinOp::And))]
    #[case('👍', Token::True)]
    #[case('👎', Token::False)]
    #[case('a', Token::Formula('a'))]
    fn test_token_try_from_char_ok(#[case] c: char, #[case] expected: Token) {
        assert_eq!(Token::try_from(c).unwrap(), expected);
    }

    #[rstest]
    fn test_token_try_from_char_err() {
        assert!(Token::try_from('.').is_err());
    }

    #[rstest]
    #[case("", vec![])]
    #[case(
        "😮 S 👏👎   🚫  😶",
        vec![
            Token::OpenParen,
            Token::Formula('S'),
            Token::BinOp(BinOp::And),
            Token::False,
            Token::UnOp(UnOp::Not),
            Token::CloseParen,
            ],
    )]
    fn test_token_lex_ok(#[case] input: &str, #[case] expected: Vec<Token>) {
        assert_eq!(lex(input).unwrap(), expected);
    }

    #[rstest]
    fn test_token_lex_err() {
        assert!(lex("😮 S 👏 . 😶").is_err());
    }
}
