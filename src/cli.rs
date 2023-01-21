//! Command line argument parsing, command execution, and printing functionality.
use std::{
    error::Error,
    fs::File,
    io::{stdin, stdout, Read, Result, Write},
};

use clap::{Parser, Subcommand};
use yansi::Paint;
use truemoji_core::{lex::lex, parse::parse};

use crate::table::generate_table;

/// Commands for the command line tool.
#[derive(Subcommand, Debug, Clone)]
pub enum Command {
    /// Generate truth tables for statements.
    Table {
        /// Format output as CSV.
        #[arg(short, long)]
        csv: bool,
    },
}

/// Command line arguments.
#[derive(Debug, Parser, Clone)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    /// The specific command to execute.
    #[command(subcommand)]
    pub command: Command,

    /// Optional input file.
    #[arg(short, long)]
    pub input: Option<String>,

    /// Optional output file.
    #[arg(short, long)]
    pub output: Option<String>,
}

impl Args {
    /// Read user input based on the command line arguments.
    ///
    /// # Returns
    /// The input string from either the user's specified file or from `stdin`.
    pub fn read_input(&self) -> Result<String> {
        let mut output = String::new();
        let mut reader: Box<dyn Read> = match &self.input {
            Some(path) => Box::new(File::open(path)?),
            None => Box::new(stdin()),
        };
        reader.read_to_string(&mut output)?;
        Ok(output)
    }

    pub fn write_handle(&self) -> Result<Box<dyn Write>> {
        Ok(match &self.output {
            Some(path) => Box::new(File::create(path)?),
            None => Box::new(stdout()),
        })
    }

    pub fn execute_cmd(&self) {
        match self.command {
            Command::Table { csv } => {
                self.table_cmd(csv);
            }
        }
    }

    fn table_cmd(&self, csv: bool) {
        match self.read_input() {
            Ok(input) => match lex(&input) {
                Ok(tokens) => match parse(&tokens) {
                    Ok(ast) => match generate_table(&input, &ast) {
                        Ok(table) => match self.write_handle() {
                            Ok(mut handle) => {
                                match csv {
                                    true => {
                                        if let Err(err) = table.to_csv(handle.as_mut()) {
                                            Self::display_err("Write Error", err)
                                        }
                                    }
                                    false => {
                                        if let Err(err) = table.print(handle.as_mut()) {
                                            Self::display_err("Write Error", err)
                                        }
                                    }
                                }
                            }
                            Err(err) => Self::display_err("Output Error", err),
                        },
                        Err(err) => Self::display_err("Evaluation Error", err),
                    },
                    Err(err) => Self::display_err("Parsing Error", err),
                },
                Err(err) => Self::display_err("Lexing Error", err),
            },
            Err(err) => Self::display_err("Input Error", err),
        };
    }

    fn display_err(name: &str, err: impl Error) {
        println!("{} {}", Paint::red(format!("[{}]", name)).bold(), err);
    }
}


