//! Generate truth tables.
use std::collections::{HashMap, HashSet};

use itertools::Itertools;
use prettytable::{Cell, Row, Table};
use rayon::prelude::*;
use truemoji_core::{
    ast::{EvalErr, Node},
    lex::Token,
};

pub fn generate_table(input: &str, ast: &Node) -> Result<Table, EvalErr> {
    let mut formulas = HashSet::new();
    ast.collect_formulas(&mut formulas);
    let formulas: Vec<char> = formulas.into_iter().sorted().par_bridge().collect();
    let rows: Result<Vec<Row>, EvalErr> = generate_rows(&ast, &formulas);
    let mut table = Table::init(rows?);
    table.set_titles(generate_title(&formulas, &input));
    Ok(table)
}

fn generate_models(formulas: &[char]) -> impl Iterator<Item = HashMap<char, bool>> + '_ {
    (0..formulas.len())
        .map(|_| [true, false])
        .multi_cartesian_product()
        .map(move |m| generate_model(&m, &formulas))
}

fn generate_model(values: &[bool], formulas: &[char]) -> HashMap<char, bool> {
    formulas
        .into_par_iter()
        .zip_eq(values.into_par_iter())
        .map(|(c, v)| (*c, *v))
        .collect()
}

fn generate_rows(ast: &Node, formulas: &[char]) -> Result<Vec<Row>, EvalErr> {
    generate_models(&formulas)
        .par_bridge()
        .map(|m| generate_row(&ast, &formulas, &m))
        .collect()
}

fn generate_row(
    ast: &Node,
    formulas: &[char],
    model: &HashMap<char, bool>,
) -> Result<Row, EvalErr> {
    let result = ast.evaluate(&model)?;
    let mut row = Row::empty();
    for f in formulas {
        let repr = match model[f] {
            true => Token::True,
            false => Token::False,
        };
        row.add_cell(Cell::new(&format!("{}", repr)));
    }
    let result_repr = match result {
        true => Token::True,
        false => Token::False,
    };
    row.add_cell(Cell::new(&format!("{}", result_repr)));
    Ok(row)
}

fn generate_title(formulas: &[char], statement: &str) -> Row {
    let mut row = Row::empty();
    for f in formulas {
        row.add_cell(Cell::new(&f.to_string()));
    }
    row.add_cell(Cell::new(statement));
    row
}
